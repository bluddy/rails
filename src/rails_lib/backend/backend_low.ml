
open Containers
open Backend_d
open Utils.Infix

let src = Logs.Src.create "backend_low" ~doc:"Backend_low"
module Log = (val Logs.src_log src: Logs.LOG)

module TS = Trackmap.Search
module G = Track_graph
module C = Constants

(* Low-level backend module. Deals with multiple modules at a time *)

module Train_update = struct

  let _update_train_target_speed (v:t) (train:Train.t) (track:Track.t) ~idx ~cycle ~x ~y ~dir =
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
    | WaitingAtStation _ -> ()
    end;
    (* Bookkeeping *)
    let dist = if Dir.is_diagonal dir then 2 else 3 in
    Train.add_dist_traveled train dist v.fiscal_period;
    update_player v train.player (Player.incr_dist_traveled ~dist);
    Train.advance train


  let _train_enter_station (v:t) loc (station:Station.t) (train:Train.t) =
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
      let cars_delivered =
        List.map (fun car -> 
          Train.Car.get_amount car > 0 && 
          Station.has_demand_for station_info car.good)
        cars
      in
      let money_from_goods =
        List.map2 (fun car delivered ->
          if delivered then
            Train.calc_arrival_money ~loc ~train ~car ~rates:station_info.rates ~region:v.region
            ~west_us_route_done:v.west_us_route_done ~year:v.year ~year_start:v.year_start
            ~difficulty:v.options.difficulty ~cycle:v.cycle
          else 0)
        cars
        cars_delivered
      in
      let other_income =
        let has_rest = Station.has_restaurant station in
        let has_hotel = Station.has_hotel station in
        List.fold_left2 (fun acc car delivered -> 
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
        cars
        cars_delivered
      in
      let station_supply = station_info.supply in
      let conversion_goods =
        List.map2 (fun car delivered -> 
          if delivered then 
            let conv_good =
              Station.convert station_info (Train.Car.get_good car) v.region
            in
            match conv_good with
            | Some good -> Some (good, Train.Car.get_amount car)
            | _ -> None
          else None)
        cars
        cars_delivered
      in
      List.iter (function
        | Some (good, amount) ->
            Hashtbl.incr station_supply good ~by:amount
        | _ -> ())
        conversion_goods;
      let time_for_sold_goods =
        List.fold_left2 (fun acc car delivered ->
          if delivered then 
            let freight = Train.Car.get_freight car |> Goods.freight_to_enum in
            acc + (freight * (Train.Car.get_amount car) / 32)
          else
            acc)
        0
        cars
        cars_delivered
      in
      let cars = List.map2 (fun car delivered ->
        if delivered then Train.Car.empty car else car)
        cars cars_delivered
      in
      let car_change_work, car_change_expense, cars =
        Train_station.dump_unused_cars_to_station cars (Train.get_stop train) station_supply
      in
      let time_for_car_change =
        let multiplier = if Station.has_upgrade station Station.SwitchingYard then 16 else 64 in
        car_change_work * multiplier
      in
      let freight = Train.freight_of_cars cars in
      let time_pickup, cars =
        Train_station.fill_train_and_empty_station cars loc v.cycle station_supply in
      let wait_time = time_for_sold_goods + time_for_car_change + time_pickup in
      let income = (Utils.sum money_from_goods) + other_income + car_change_expense in
      let state =
        if wait_time > 0 then Train.WaitingAtStation {wait_time}
        else train.state
      in
      Log.debug (fun f -> f "wait_time(%d)" wait_time);
      {train with cars; had_maintenance; state; freight}, income, []
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

  let _update_train_mid_tile ~idx ~cycle (v:t) (train:Train.t) loc =
    (* All major computation happens mid-tile *)
    let (x,y) = loc in
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
        let station = Station_map.get_exn loc v.stations in
        let enter train =
          begin match train.Train.segment with
          | Some segment ->
              (* exit segment *)
              Segment.Map.decr_train v.segments segment
          | _ -> ()
          end;
          (* TODO: actual UI msg, income handling *)
          let last_station, priority, stop, train, _income, _ui_msgs =
            if Station.is_proper_station station then (
              let train, income, ui_msgs = _train_enter_station v loc station train in
              let priority, stop = Train.check_increment_stop train loc in
              loc, priority, stop, train, income, ui_msgs
            ) else (
              train.last_station, train.priority, train.stop, train, 0, []
            )
          in
          {train with segment=None; last_station; priority; stop}
        in
        let exit train =
          let dest = Train.get_dest train in
          let dir =
            match Track_graph.shortest_path v.graph ~src:loc ~dest with
            | Some dir -> dir
            | None -> (* TODO: Impossible route message *)
              Dir.Set.find_nearest train.dir track.dirs
              |> Option.get_exn_or "Cannot find track for train"
          in
          let segment = Station.get_segment station dir in
          Segment.Map.incr_train v.segments segment;
          (* TODO Check signal for exit dir *)
          let train = 
            {train with segment=Some segment;
             state=Train.Traveling {speed=0; target_speed=4; last_stop=loc}}
          in
          _update_train_target_speed v train track ~idx ~cycle ~x ~y ~dir
        in
        let train = match train.state with
          | Traveling _ ->
              let train = enter train in
              (match train.state with
              | WaitingAtStation s when s.wait_time > 0 -> train
              | _ -> exit train)
          | WaitingAtStation s when s.wait_time > 0 -> train
          | WaitingAtStation _ -> exit train
        in
        Log.debug (fun f -> f "Station: %s" (Train.show_state train.state));
        train

    | Track _ when track.ixn && Dir.Set.num_adjacent train.dir track.dirs > 1 ->
        (* ixn *)
        let dir =
          let dest = Train.get_dest train in
          Track_graph.shortest_path_branch v.graph
            ~ixn:loc ~cur_dir:train.dir ~dest 
            |> Option.get_exn_or "Cannot find route for train" 
        in
        _update_train_target_speed v train track ~idx ~cycle ~x ~y ~dir

    | _ ->
        (* All other track and non-decision ixns *)
        let dir = 
          Dir.Set.find_nearest train.dir track.dirs
          |> Option.get_exn_or "Cannot find track for train"
        in
        _update_train_target_speed v train track ~idx ~cycle ~x ~y ~dir

    (* Run every cycle, updating every train's position and speed *)
  let _update_all_trains (v:t) =
    (* Log.debug (fun f -> f "update_all_trains"); *)
    let cycle_check, region_div = if Region.is_us v.region then 16, 1 else 8, 2 in
    let cycle_bit = 1 lsl (v.cycle mod 12) in
    let cycle = v.cycle in
    (* TODO: We update the high priority trains before the low priority *)
    Trainmap.mapi_in_place (fun idx train ->
      Train.update_train idx train ~cycle ~cycle_check ~cycle_bit ~region_div
        ~update_mid_tile:(_update_train_mid_tile ~idx ~cycle v))
    v.trains


end

(** Most time-based work happens here **)
let handle_cycle v =
  let time_step () =
    v.cycle <- v.cycle + 1;
    Train_update._update_all_trains v;
    (* TODO: ai_routines *)
    let demand_msgs =
      if v.cycle mod cycles_station_supply_demand = 0 then (
        (* Printf.printf "_handle_cycle%!\n"; *)
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
        ~init:[])
      else []
    in
    (* adjust time *)
    v.time <- v.time + 1;
    let v = 
      if v.time >= year_ticks then
        {v with year=v.year + 1; time=0}
      else v
    in
    v, demand_msgs
  in
  if not v.pause then time_step () else v, []
