
open Containers
open Backend_d
open Utils.Infix

let src = Logs.Src.create "backend_low" ~doc:"Backend_low"
module Log = (val Logs.src_log src: Logs.LOG)

module G = Track_graph
module C = Constants

(* Low-level backend module. Deals with multiple modules at a time *)

module Train_update = struct

  let _update_train_target_speed (v:t) (train:rw Train.t) (track:Track.t) ~idx ~cycle ~x ~y ~dir =
    (* Speed factor computation from height delta and turn *)
    let height1 = Tilemap.get_tile_height v.map x y in
    let x2, y2 = Dir.adjust dir x y in
    let track2 = Trackmap.get_exn v.track ~x:x2 ~y:y2 in
    let height2 = Tilemap.get_tile_height v.map x2 y2 in
    let d_height = max 0 (height2 - height1) in
    let d_height = if Dir.is_diagonal dir then d_height else d_height * 3/2 in
    let d_height = if B_options.easy v.options.difficulty then d_height/2 else d_height in 
    let height_factor = match track.kind, track2.kind with
      | Bridge _ , _ -> 0
      | _, Tunnel -> 0
      | _ -> d_height
    in
    let turn_factor = Dir.diff dir train.dir in
    let speed_factor = (height_factor * height_factor / 144) + turn_factor in
    Log.debug (fun f -> f "height_factor(%d), turn_factor(%d), speed_factor(%d)" height_factor turn_factor speed_factor);
    (* History lets us reuse the engine orientation for the cars as they cross *)
    Train.History.add train.history train.x train.y dir speed_factor;
    (* Compute and set target speed *)
    let target_speed, speed =
      match Tilemap.get_tile v.map x y with
      | Ocean _ | Harbor _ -> 1, 1 
      | _ -> Train.compute_target_speed train ~idx ~cycle, Train.get_speed train
    in
    (* updates *)
    Train.reset_pixels_from_midtile train;
    if dir =!= train.dir then train.dir <- dir;
    begin match train.state with
    | Traveling s ->
      if target_speed =!= s.target_speed then s.target_speed <- target_speed;
      if speed =!= s.speed then s.speed <- speed;
    | _ -> ()
    end;
    (* Bookkeeping *)
    let dist = if Dir.is_diagonal dir then 2 else 3 in
    Train.add_dist_traveled train dist v.fiscal_period;
    update_player v train.player (Player.incr_dist_traveled ~dist);
    Train.advance train


  let _train_station_handle_consist_and_maintenance (v:t) idx loc (station:Station.t) (train:rw Train.t) =
    (* returns train, income, ui_msgs *)
    let handle_stop station_info =
      let had_maintenance =
        if Station.can_maintain station then true
        else train.had_maintenance
      in
      (* TODO: deal with priority shipment *)
      (* TODO: deal with dist_shipped_cargo *)
      let _dist_shipped_cargo =
        let dist = Utils.classic_dist loc train.last_station in
        let num_cars = List.length train.cars in
        let total_dist = dist * num_cars in
        Train.add_ton_miles train total_dist v.fiscal_period
      in
      let cars = train.cars in

      let deliver_cargo () =
        let cars_delivered =
          List.map (fun car -> 
            car,
            Train.Car.get_amount car > 0 && 
            Station.has_demand_for station_info car.good)
          cars
        in
        let money_from_goods =
          List.map (fun (car, delivered) ->
            if delivered then
              Train.calc_arrival_money ~loc ~train ~car ~rates:station_info.rates ~region:v.region
              ~west_us_route_done:v.west_us_route_done ~year:v.year ~year_start:v.year_start
              ~difficulty:v.options.difficulty ~cycle:v.cycle
            else 0)
          cars_delivered
        in
        let other_income =
          let has_rest = Station.has_restaurant station in
          let has_hotel = Station.has_hotel station in
          List.fold_left (fun acc (car, delivered) -> 
            let amount, good = Train.Car.get_amount car, Train.Car.get_good car in
            match good with
            | Passengers when delivered && has_rest && has_hotel ->
                acc + amount/32 + amount/16
            | Passengers when delivered && has_rest ->
                acc + amount / 32
            | Passengers when delivered && has_hotel ->
                acc + amount / 16
            | _ -> acc)
          0
          cars_delivered
        in
        cars_delivered, money_from_goods, other_income
      in
      let cars_delivered, money_from_goods, other_income = deliver_cargo () in

      let station_supply = station_info.supply in
      let add_converted_goods_to_station cars_delivered =
        let conversion_goods =
          List.map (fun (car, delivered) -> 
            if delivered then 
              let conv_good =
                Station.convert station_info (Train.Car.get_good car) v.region
              in
              match conv_good with
              | Some good -> Some (good, Train.Car.get_amount car)
              | _ -> None
            else None)
          cars_delivered
        in
        (* Add converted goods to station *)
        List.iter (function
          | Some (good, amount) ->
              Hashtbl.incr station_supply good ~by:amount
          | _ -> ())
          conversion_goods
      in
      add_converted_goods_to_station cars_delivered;

      let time_for_sold_goods =
        List.fold_left (fun acc (car, delivered) ->
          if delivered then 
            let freight = Train.Car.get_freight car |> Freight.to_enum in
            acc + (freight * (Train.Car.get_amount car) / 32)
          else
            acc)
        0
        cars_delivered
      in
      (* Refresh cars: empty cars emptied *)
      let cars = List.map (fun (car, delivered) ->
        if delivered then Train.Car.empty car else car)
        cars_delivered
      in
      let _, next_stop = Train.get_next_stop train in
      let car_change_work, car_change_expense, cars =
        Train_station.dump_unused_cars_to_station cars next_stop station_supply
      in
      let time_for_car_change =
        let multiplier = if Station.has_upgrade station Station.SwitchingYard then 16 else 64 in
        car_change_work * multiplier
      in
      let freight = Train.freight_of_cars cars in
      let time_for_pickup, cars =
        Train_station.train_pickup_and_empty_station cars loc v.cycle station_supply in

      let wait_time = time_for_sold_goods + time_for_car_change + time_for_pickup in

      (* This function always naively switches to loading at station. Other conditions will be handled elsewhere *)
      let state = Train.LoadingAtStation {wait_time} in

      let revenue = (Utils.sum money_from_goods) + other_income - car_change_expense in

      let goods_delivered = List.fold_left (fun acc (car, delivered) ->
        if delivered then
          let good, amount = Train.Car.get_good car, Train.Car.get_amount car in
          Goods.Map.add good amount acc
        else acc)
        Goods.Map.empty
        cars_delivered
      in
      let goods_delivered_amt = Goods.Map.to_list goods_delivered in
      let total_goods = List.fold_left (fun acc (_,amt) -> acc + amt) 0 goods_delivered_amt in
      let ui_msgs =
        if total_goods > 0 then (
          let complex_freight = Train.freight_set_of_cars train.cars |> Freight.complex_of_set in
          [TrainArrival {
            player=0;
            time=v.time;
            freight=complex_freight;
            _type=train.typ;
            train_num=idx;
            train_name = train.name;
            revenue;
            goods_amount=goods_delivered_amt;
          }]
        ) else []
      in
      Log.debug (fun f -> f "Wait_time(%d)" wait_time);

      {train with cars; had_maintenance; state; freight}, revenue, ui_msgs
    in
    match station.info with
    | Some station_info when Train_station.train_stops_at station train ->
          handle_stop station_info
    | Some _ when not train.had_maintenance && Station.can_maintain station ->
         {train with had_maintenance=true}, 0, []
    | _ -> train, 0, []


      (* TODO: young_station_reached if age <= 20 *)
      (* add income/2 to other_income type *)
      (* add to dist_shipped_cargo *)
      (* check record delivery money-wise *)
      (* check conversion and add to station *)
      (* compute and track money *)
      (* add to time at station *)

      (* adjust: 
        (* Update goods_shipped *)
        (* Update freight_shipped *)
        update goods_shipped_dist with station_dist * cars
        add converted goods
        money to income type, including hotel and restaurant
        car goods level
        station's cargo_money_array
        train's money
        train's fiscal period money
        pickup from young station: affects pixel change algorithm
        *)
      (* ui msgs: 
         first delivery of good (only >2 difficulty)
         first pickup of goods (only >2 difficulty)
         record reward for delivery
         delivery to station, print revenue
       *)

  let _enter_station (v:t) idx (train: rw Train.t) station loc  =
    (* TODO: income handling *)
    let last_station, priority, stop, train, _income, ui_msgs =
      if Station.is_proper_station station then (
        let train, income, ui_msgs =
          _train_station_handle_consist_and_maintenance v idx loc station train in
        let priority, stop = Train.check_increment_stop train loc in
        loc, priority, stop, train, income, ui_msgs
      ) else (
        (* Just a signal tower. Keep traveling *)
        train.last_station, train.priority, train.stop, train, 0, []
      )
    in
    {train with last_station; priority; stop}, ui_msgs

  let _exit_station ~idx ~cycle (v:t) (train: rw Train.t) (track:Track.t) ((x, y) as loc) =
    let compute_dir_to_dest graph =
      (* This is expensive *)
      let dest = Train.get_dest train in
      let dir =
        match Track_graph.shortest_path graph ~src:loc ~dest with
        | Some dir -> dir
        | None -> (* TODO: Impossible route message *)
          Dir.Set.find_nearest train.dir track.dirs
          |> Option.get_exn_or "Cannot find track for train"
      in
      dir
    in
    let dir = compute_dir_to_dest v.graph in
    let station = Station_map.get_exn loc v.stations in
    (* Second check of this. No matter *)
    let can_go, cancel_override = Station.can_train_go station dir in
    if can_go then (
      (* Mutably canel override if needed. Stringing the station along
         right now would be painful *)
      begin match cancel_override with
      | `Cancel_override -> Station.cancel_override_mut station dir
      | _ -> ()
      end;
      (* enter block *)
      let locd = (loc, dir) in
      let locu = Utils.locu_of_locd locd in
      let block = Block_map.block_incr_train locu v.blocks in
      let train = 
        {train with
          state=Train.Traveling {speed=0; target_speed=4; traveling_past_station=true; block}
        }
      in
      _update_train_target_speed v train track ~idx ~cycle ~x ~y ~dir
    ) else (
      {train with state=Train.StoppedAtSignal dir}
    )

  let _handle_train_mid_tile ~idx ~cycle (v:t) (train:rw Train.t) ((x, y) as loc) =
    (* All major computation happens mid-tile *)
    (* Log.debug (fun f -> f "_update_train_mid_tile"); *)
    (* TODO: check for colocated trains (accidents/stop a train) *)
    (* Trains can be stopped by 3 things:
      1. R-click: told to stop at next stop
         Don't process train arrival
      2. Wait timer from first arrival
      3. Prevent from leaving via manual signal hold
    *)
    let track = Trackmap.get_exn v.track ~x ~y in
    match track.kind with
    | Station _ ->
        (* TODO: remove override Proceed after one train *)
        let train, ui_msgs = match train.state with
            (* This is only when we've already processed the train *)
          | Traveling s when s.traveling_past_station ->
              train, []

            (* This is before possibly entering the station *)
          | Traveling s ->
              Block_map.block_decr_train s.block v.blocks;
              let train, ui_msgs =
                if train.hold_at_next_station then (
                {train with state = HoldingAtStation}, []
                ) else (
                  let station = Station_map.get_exn loc v.stations in
                  _enter_station v idx train station loc
                )
              in
              if Train.is_traveling train then
                (* No stopping at this station *)
                _exit_station ~idx ~cycle v train track loc, ui_msgs
              else
                (* Some kind of stop. Exit later *)
                train, ui_msgs

            (* Loading/unloading goods at the station *)
          | LoadingAtStation s when s.wait_time > 0 ->
              s.wait_time <- s.wait_time - 1;
              train, []

          | LoadingAtStation _ ->
              (* Done loading/unloading. Check if we can exit the station *)
              let wait_at_stop, _ = Train.get_next_stop train in
              let train = match wait_at_stop with
                | `Wait -> {train with state=Train.WaitingForFullLoad}
                | _ -> _exit_station ~idx ~cycle v train track loc
              in
              train, []

          | WaitingForFullLoad when Train.is_full train ->
              (* Done waiting for full load *)
              _exit_station ~idx ~cycle v train track loc, []

          | WaitingForFullLoad ->
              (* If we're not full, we need to see if we can offload more from the station *)
              let wait_time, cars =
                let station = Station_map.get_exn loc v.stations in
                let station_supply = Station.get_supply_exn station in
                Train_station.train_pickup_and_empty_station train.cars loc v.cycle station_supply
              in
              if wait_time > 0 then
                (* We found stuff to load *)
                {train with state = LoadingAtStation {wait_time}; cars}, []
              else
                (* Keep waiting for more goods to show up *)
                [%up {train with cars}], []

          | HoldingAtStation when train.hold_at_next_station ->
              (* Don't enter station yet *)
              train, []

          | HoldingAtStation ->
              (* Hold happens before we enter the station *)
              let station = Station_map.get_exn loc v.stations in
              _enter_station v idx train station loc

          | StoppedAtSignal dir ->
              (* This happens after we've already 'exited' *)
              let station = Station_map.get_exn loc v.stations in
              (* Check here as well to avoid expensive computation *)
              let can_go, _ = Station.can_train_go station dir in
              if can_go then
                _exit_station ~idx ~cycle v train track loc, []
              else
                train, []
        in
        Log.debug (fun f -> f "Train at station: %s" (Train.show_state train.state));
        train, ui_msgs

    (* --- Below this trains cannot stop so must be traveling --- *)

    | Track _ when track.ixn && Dir.Set.num_adjacent train.dir track.dirs > 1 ->
        (* ixn *)
        let dir =
          let dest = Train.get_dest train in
          Track_graph.shortest_path_branch v.graph
            ~ixn:loc ~cur_dir:train.dir ~dest 
            |> Option.get_exn_or "Cannot find route for train" 
        in
        _update_train_target_speed v train track ~idx ~cycle ~x ~y ~dir, []

    | _ ->
        (* All other track and non-decision ixns *)
        let dir = 
          Dir.Set.find_nearest train.dir track.dirs
          |> Option.get_exn_or "Cannot find track for train"
        in
        _update_train_target_speed v train track ~idx ~cycle ~x ~y ~dir, []

let update_train v idx (train:rw Train.t) ~cycle ~cycle_check ~cycle_bit ~region_div =
  (* This is the regular update function for trains. Important stuff happens midtile *)
  (* let priority = (Goods.freight_to_enum train.freight) * 3 - (Train.train_type_to_enum train.typ) + 2 in *)
  begin match train.state with
  | Traveling travel_state ->
    let train = Train.update_speed train ~cycle ~cycle_check ~cycle_bit in
    (* TODO: fiscal period update stuff *)
    let rec train_update_loop train speed_bound ui_msg_acc =
      let speed = Train.get_speed train in
      if speed_bound >= speed then train, ui_msg_acc
      else (
        let speed =
          if Dir.is_diagonal train.dir then (speed * 2 + 1) / 3
          else speed
        in
        let update_val =
          if speed > 12 then 
            if speed_bound = 0 then 12 else speed - 12
          else speed
        in
        (* BUGFIX: original code allowed sampling from random memory *)
        let update_val = update_val / region_div
          |> min Train.update_array_length
        in
        (* Log.debug (fun f -> f "Update val %d, cycle_bit %d" update_val cycle_bit); *)
        let train, ui_msgs =
          if (Train.update_cycle_array.(update_val) land cycle_bit) <> 0 then begin
            (* Log.debug (fun f -> f "Pass test. Update val %d, cycle_bit %d" update_val cycle_bit); *)
            let is_mid_tile =
              (train.x mod C.tile_w) = C.tile_w / 2 &&
              (train.y mod C.tile_h) = C.tile_h / 2
            in
            let loc = train.x / C.tile_w, train.y / C.tile_h in

            (* Make sure we don't double-process mid-tiles *)
            match is_mid_tile, travel_state.traveling_past_station with
            | true, false ->
                _handle_train_mid_tile ~idx ~cycle v train loc
            | false, true ->
                travel_state.traveling_past_station <- false;
                Train.advance train, []
            | _ ->
                Train.advance train, []
          end else
              train, []
        in
        train_update_loop train (speed_bound + 12) (ui_msgs @ ui_msg_acc)
      )
    in
    train_update_loop train 0 []

  | _ ->  (* Other train states or time is up *)
    let loc = train.x / C.tile_w, train.y / C.tile_h in
    _handle_train_mid_tile ~idx ~cycle v train loc
  end


    (* Run every cycle, updating every train's position and speed *)
  let _update_all_trains (v:t) =
    (* Log.debug (fun f -> f "update_all_trains"); *)
    let cycle_check, region_div = if Region.is_us v.region then 16, 1 else 8, 2 in
    let cycle_bit = 1 lsl (v.cycle mod 12) in
    let cycle = v.cycle in
    (* TODO: We update the high priority trains before the low priority *)
    Trainmap.fold_mapi_in_place (fun idx ui_msg_acc train ->
      let train, ui_msgs = 
        update_train v idx train ~cycle ~cycle_check ~cycle_bit ~region_div
      in
      ui_msgs @ ui_msg_acc, train)
      v.trains
      ~init:[]

end

(** Most time-based work happens here **)
let handle_cycle v =
  let time_step () =
    v.cycle <- v.cycle + 1;
    let ui_msgs = Train_update._update_all_trains v in
    (* TODO: ai_routines *)
    let ui_msgs =
      if v.cycle mod cycles_station_supply_demand = 0 then (
        let difficulty = v.options.difficulty in
        let climate = v.climate in
        let simple_economy =
          not @@ B_options.RealityLevels.mem v.options.reality_levels `ComplexEconomy 
        in
        Station_map.fold 
          (fun station old_msgs ->
            Station.check_rate_war_lose_supplies station ~difficulty;
            let msgs =
              Station.update_supply_demand station v.map ~climate ~simple_economy
            in
            Station.lose_supplies station;
            let msgs =
              List.map (fun (good, add) ->
                  DemandChanged {x=station.x; y=station.y; good; add})
                msgs
            in
            msgs @ old_msgs)
        v.stations
        ~init:ui_msgs)
      else ui_msgs
    in
    (* adjust time *)
    v.time <- v.time + 1;
    let v = 
      if v.time >= Constants.year_ticks then
        {v with year=v.year + 1; time=0}
      else v
    in
    v, ui_msgs
  in
  if not v.pause then time_step () else v, []

