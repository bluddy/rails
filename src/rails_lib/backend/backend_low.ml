
open Containers
open Backend_d
open Utils.Infix

let src = Logs.Src.create "backend_low" ~doc:"Backend_low"
module Log = (val Logs.src_log src: Logs.LOG)

module G = Track_graph
module C = Constants
module IS = Income_statement
module UIM = Ui_msg

(* Low-level backend module. Deals with multiple modules at a time *)

module Train_update = struct

  let _update_train_target_speed (v:t) (train:rw Train.t) (track:Track.t) ~idx ~cycle loc ~dir =
    (* Speed factor computation from height delta and turn *)
    let height1 = Tilemap.get_tile_height loc v.map in
    let loc2 = Dir.adjust_loc dir loc in
    let track2 = Trackmap.get_exn loc2 v.track in
    let height2 = Tilemap.get_tile_height loc2 v.map in
    let d_height = max 0 (height2 - height1) in
    let d_height = if Dir.is_diagonal dir then d_height else d_height * 3/2 in
    let d_height = if B_options.easy v.params.options.difficulty then d_height/2 else d_height in 
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
      match Tilemap.get_tile loc v.map with
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
    Train.add_dist_traveled dist v.params.fiscal_period train;
    update_player_state v train.player (Player.incr_dist_traveled ~dist);
    Train.advance train


  let _train_station_handle_consist_and_maintenance (v:t) idx loc
      (station:Station.t) (train:rw Train.t) =
    let handle_stop station_info =
      let had_maintenance = Station.can_maintain station || train.had_maintenance in
      (* Priority shipment arriving to station *)
      let station =
        if Train.holds_priority_shipment train && not @@ Station.holds_priority_shipment station then
          Station.set_priority_shipment station true
        else station
      in
      let ton_miles =
        let dist = Utils.classic_dist loc train.last_station in
        let num_cars = List.length train.cars in
        dist * num_cars
        (* Train.add_ton_miles train total_dist v.fiscal_period *)
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
          List.fold_left (fun acc (car, delivered) ->
            if delivered then
              let money =
                Train.car_delivery_money ~loc ~train ~car ~rates:station_info.rates ~params:v.params
              in
              let freight = Freight.of_good car.good in
              IS.RevenueMap.incr freight money acc
            else acc)
          IS.RevenueMap.empty
          cars_delivered
        in
        let freight_ton_miles =
          List.fold_left (fun acc (car, delivered) ->
            if delivered then
              let ton_miles = Train.car_delivery_ton_miles ~loc ~car ~region:v.params.region in
              let freight = Freight.of_good car.good in
              Freight.Map.incr freight ton_miles acc
            else acc)
          Freight.Map.empty
          cars_delivered
        in
        let other_income =
          let has_rest = Station.has_restaurant station in
          let has_hotel = Station.has_hotel station in
          List.fold_left (fun acc (car, delivered) -> 
            let amount, good = Train.Car.get_amount car, Train.Car.get_good car in
            match good with
            | Passengers when delivered && has_rest && has_hotel ->
                acc + amount / 32 + amount / 16
            | Passengers when delivered && has_rest ->
                acc + amount / 32
            | Passengers when delivered && has_hotel ->
                acc + amount / 16
            | _ -> acc)
          0
          cars_delivered
        in
        cars_delivered, money_from_goods, freight_ton_miles, other_income
      in
      let cars_delivered, money_from_goods, freight_ton_miles, other_income = deliver_cargo () in

      let station_supply = station_info.supply in
      let add_converted_goods_to_station cars_delivered =
        let conversion_goods =
          List.map (fun (car, delivered) -> 
            if delivered then 
              let conv_good =
                Station.convert station_info (Train.Car.get_good car) v.params.region
              in
              match conv_good with
              | Some good -> Some (good, Train.Car.get_amount car)
              | _ -> None
            else None)
          cars_delivered
        in
        (* Add converted goods to station *)
        List.iter (function
          | Some (good, amount) -> Hashtbl.incr station_supply good ~by:amount
          | _ -> ())
          conversion_goods
      in
      add_converted_goods_to_station cars_delivered;

      let time_for_sold_goods =
        List.fold_left (fun acc (car, delivered) ->
          if delivered then 
            let freight = Train.Car.get_freight car |> Freight.to_enum in
            acc + (freight * (Train.Car.get_amount car) / 32)
          else acc)
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
      let time_for_pickup, cars, station =
        Train_station.train_pickup_and_empty_station cars loc v.params.cycle station
      in
      (* Update whether we leave with priority shipment on train *)
      let holds_priority_shipment =
        let player = Player.get train.player v.players in
        match Player.get_priority player with
        | None -> false
        | Some priority_shipment ->
          let freight = Priority_shipment.get_freight priority_shipment in
          Station.holds_priority_shipment station &&
          Train.can_hold_priority_shipment cars freight
      in

      let wait_time = time_for_sold_goods + time_for_car_change + time_for_pickup in
      let economic_activity = time_for_sold_goods > 0 || time_for_pickup > 0 || train.economic_activity in

      (* This function always naively switches to loading at station. Other conditions will be handled elsewhere *)
      let state = Train.LoadingAtStation {wait_time} in

      let goods_revenue = IS.RevenueMap.total money_from_goods in
      let revenue = goods_revenue + other_income - car_change_expense in

      let periodic = Train.update_periodic v.params.fiscal_period train.periodic
        (fun p -> {p with ton_miles=p.ton_miles + ton_miles; revenue=p.revenue + goods_revenue;})
      in

      let income_stmt =
        IS.default
        |> IS.add_revenues money_from_goods 
        |> IS.add_revenue `Other other_income
        |> IS.deduct `Train car_change_expense
      in

      let goods_delivered = List.fold_left (fun acc (car, delivered) ->
        if delivered then
          let good, amount = Train.Car.get_good car, Train.Car.get_amount car in
          Goods.Map.incr good amount acc
        else acc)
        Goods.Map.empty
        cars_delivered
      in
      let goods_delivered_amt = Goods.Map.to_list goods_delivered in
      let total_goods = Goods.Map.total goods_delivered in
      let ui_msgs, data =
        if total_goods > 0 then
          let complex_freight = Train.freight_set_of_cars train.cars |> Freight.complex_of_set in
          let msg =
            UIM.TrainArrival {
              player=train.player;
              time=v.params.time;
              freight=complex_freight;
              _type=train.typ;
              train_num=idx;
              train_name = train.name;
              revenue;
              goods_amount=goods_delivered_amt;
            }
          in
          let data = (income_stmt, freight_ton_miles) in
          [msg], Some data
        else
          [], None
      in
      Log.debug (fun f -> f "Wait_time(%d)" wait_time);

      let train = {
        train with cars;
        had_maintenance;
        state;
        freight;
        holds_priority_shipment;
        economic_activity;
        periodic;
      }
      in
      train, station, data, ui_msgs
    in
    match station.info with
    | Some station_info when Train_station.train_stops_at station train ->
          handle_stop station_info
    | Some _ when not train.had_maintenance && Station.can_maintain station ->
         {train with had_maintenance=true}, station, None, []
    | _ -> train, station, None, []


      (* TODO: young_station_reached if age <= 20 *)
      (* add income/2 to other_income type *)
      (* check record delivery money-wise *)

      (* ui msgs: 
         first delivery of good (only >2 difficulty)
         first pickup of goods (only >2 difficulty)
         record reward for delivery
       *)

  let _enter_station (v:t) idx (train: rw Train.t) stations loc  =
    let station' = Station_map.get_exn loc stations in
    let last_station, priority_stop, stop, train, station, data, ui_msgs =
      if Station.is_proper_station station' then (
        let train, station, data, ui_msgs =
          _train_station_handle_consist_and_maintenance v idx loc station' train in
        let priority_stop, stop = Train.check_increment_stop loc train in
        loc, priority_stop, stop, train, station, data, ui_msgs
      ) else (
        (* Just a signal tower. Keep traveling *)
        train.last_station, train.priority_stop, train.stop, train, station', None, []
      )
    in
    let stations = if station' =!= station then Station_map.add loc station stations else stations
    in
    let train = [%up {train with last_station; priority_stop; stop}] in
    train, stations, data, ui_msgs

  let _exit_station ~idx ~cycle (v:t) (train: rw Train.t) stations (track:Track.t) loc =
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
    let station = Station_map.get_exn loc stations in
    (* Second check of this. No matter *)
    let can_go, cancel_override = Station.can_train_go station dir in
    if can_go then (
      let stations = match cancel_override with
        | `Cancel_override ->
          let station = Station.cancel_override station dir in
          Station_map.add loc station stations
        | _ -> stations
      in
      let active_stations = if train.economic_activity then [loc] else [] in

      (* enter block *)
      let locd = (loc, dir) in
      let locu = Utils.locu_of_locd locd in
      let block = Block_map.block_incr_train locu v.blocks in
      let train = 
        {train with
          state=Train.Traveling {speed=0; target_speed=4; traveling_past_station=true; block};
          economic_activity=false; (* reset *)
        }
      in
      _update_train_target_speed v train track ~idx ~cycle loc ~dir, stations, active_stations
    ) else (
      {train with state=Train.StoppedAtSignal dir}, stations, []
    )

  let _handle_train_mid_tile ~idx ~cycle (v:t) (train:rw Train.t) stations loc =
    (* All major computation happens mid-tile *)
    (* Log.debug (fun f -> f "_update_train_mid_tile"); *)
    (* TODO: check for colocated trains (accidents/stop a train) *)
    (* Trains can be stopped by 3 things:
      1. R-click: told to stop at next stop
         Don't process train arrival
      2. Wait timer from first arrival
      3. Prevent from leaving via manual signal hold
    *)
    let track = Trackmap.get_exn loc v.track in
    let default_ret = train, stations, None, [], [] in
    match track.kind with
    | Station _ ->
        (* TODO: remove override Proceed after one train *)
        let train, stations, data, active_stations, ui_msgs = match train.state with
            (* This is only when we've already processed the train *)
          | Traveling s when s.traveling_past_station -> default_ret

            (* This is before possibly entering the station *)
          | Traveling s ->
              Block_map.block_decr_train s.block v.blocks;
              let train, stations, data, ui_msgs =
                if train.hold_at_next_station then
                  {train with state = HoldingAtStation}, stations, None, []
                else
                  _enter_station v idx train stations loc
              in
              if Train.is_traveling train then
                (* No stopping at this station *)
                let train, stations, active_stations = _exit_station ~idx ~cycle v train stations track loc in
                train, stations, data, active_stations, ui_msgs
              else
                (* Some kind of stop. Exit later *)
                train, stations, data, [], ui_msgs

            (* Loading/unloading goods at the station *)
          | LoadingAtStation s when s.wait_time > 0 ->
              s.wait_time <- s.wait_time - 1;
              default_ret

          | LoadingAtStation _ ->
              (* Done loading/unloading. Check if we can exit the station *)
              let wait_at_stop, _ = Train.get_next_stop train in
              let train, stations, active_stations = match wait_at_stop with
                | `Wait -> {train with state=Train.WaitingForFullLoad}, stations, []
                | _ -> _exit_station ~idx ~cycle v train stations track loc
              in
              train, stations, None, active_stations, []

          | WaitingForFullLoad when Train.is_full train ->
              (* Done waiting for full load *)
              let train, stations, active_stations = _exit_station ~idx ~cycle v train stations track loc in
              train, stations, None, active_stations, []

          | WaitingForFullLoad ->
              (* If we're not full, we need to see if we can offload more from the station *)
              let wait_time, cars, stations =
                let station' = Station_map.get_exn loc stations in
                let wait_time, cars, station = Train_station.train_pickup_and_empty_station train.cars loc v.params.cycle station' in
                let stations = if station =!= station' then Station_map.add loc station stations else stations in
                wait_time, cars, stations
              in
              if wait_time > 0 then
                (* We found stuff to load *)
                let train = {train with state = LoadingAtStation {wait_time}; cars; economic_activity=true} in
                train, stations, None, [], []
              else
                (* Keep waiting for more goods to show up *)
                [%up {train with cars}], stations, None, [], []

          | HoldingAtStation when train.hold_at_next_station ->
              (* Don't enter station yet *)
              default_ret

          | HoldingAtStation ->
              (* Hold happens before we enter the station *)
              let train, stations, data, ui_msgs = _enter_station v idx train stations loc in
              train, stations, data, [], ui_msgs

          | StoppedAtSignal dir ->
              (* This happens after we've already 'exited' *)
              let station = Station_map.get_exn loc v.stations in
              (* Check here as well to avoid expensive computation *)
              let can_go, _ = Station.can_train_go station dir in
              if can_go then
                let train, stations, active_stations = _exit_station ~idx ~cycle v train stations track loc in
                train, stations, None, active_stations, []
              else
                default_ret
        in
        Log.debug (fun f -> f "Train at station: %s" (Train.show_state train.state));
        train, stations, data, active_stations, ui_msgs

    (* --- Below this trains cannot stop so must be traveling --- *)

    | Track _ when track.ixn && Dir.Set.num_adjacent train.dir track.dirs > 1 ->
        (* ixn *)
        let dir =
          let dest = Train.get_dest train in
          Track_graph.shortest_path_branch v.graph
            ~ixn:loc ~cur_dir:train.dir ~dest 
            |> Option.get_exn_or "Cannot find route for train" 
        in
        let train = _update_train_target_speed v train track ~idx ~cycle loc ~dir in
        train, stations, None, [], []

    | _ ->
        (* All other track and non-decision ixns *)
        let dir = 
          Dir.Set.find_nearest train.dir track.dirs
          |> Option.get_exn_or "Cannot find track for train"
        in
        let train = _update_train_target_speed v train track ~idx ~cycle loc ~dir in
        train, stations, None, [], []

let _update_player_with_data (player:Player.t) data active_stations fiscal_period random =
    let player = match data with
      | Some (income_stmt, freight_ton_miles) ->
          Player.add_income_stmt income_stmt player
          |> Player.add_freight_ton_miles freight_ton_miles fiscal_period
      | _ -> player
    in
    if List.is_empty active_stations then player
    else
      (* Pick just one as our active station to giver a fair chance to all *)
      let active_station = Random.pick_list active_stations random in
      Player.set_active_station active_station player

let _update_train v idx (train:rw Train.t) stations (player:Player.t) ~cycle ~cycle_check ~cycle_bit ~region_div =
  (* This is the regular update function for trains. Important stuff happens midtile
     The train can be looped over several times as it picks up speed.
   *)

  (* let priority = (Goods.freight_to_enum train.freight) * 3 - (Train.train_type_to_enum train.typ) + 2 in *)
  begin match train.state with
  | Traveling travel_state ->
    let train = Train.update_speed train ~cycle ~cycle_check ~cycle_bit in
    (* TODO: fiscal period update stuff *)
    let rec train_update_loop train speed_bound stations player ui_msg_acc =
      let speed = Train.get_speed train in
      if speed_bound >= speed then train, stations, player, ui_msg_acc
      else (
        let speed =
          if Dir.is_diagonal train.dir then (speed * 2 + 1) / 3 else speed
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
        let train, stations, data, active_stations, ui_msgs =
          if (Train.update_cycle_array.(update_val) land cycle_bit) <> 0 then begin
            let is_mid_tile =
              (train.x mod C.tile_w) = C.tile_w / 2 &&
              (train.y mod C.tile_h) = C.tile_h / 2
            in
            let loc = train.x / C.tile_w, train.y / C.tile_h in

            (* Make sure we don't double-process mid-tiles *)
            match is_mid_tile, travel_state.traveling_past_station with
            | true, false ->
                _handle_train_mid_tile ~idx ~cycle v train stations loc
            | false, true ->
                travel_state.traveling_past_station <- false;
                Train.advance train, stations, None, [], []
            | _ ->
                Train.advance train, stations, None, [], []
          end else
            train, stations, None, [], []
        in
        let player = _update_player_with_data player data active_stations v.params.fiscal_period v.random in
        train_update_loop train (speed_bound + 12) stations player (ui_msgs @ ui_msg_acc)
      )
    in
    train_update_loop train 0 stations player []

  | _ ->  (* Other train states or time is up *)
    let loc = train.x / C.tile_w, train.y / C.tile_h in
    let train, stations, data, active_stations, ui_msgs = _handle_train_mid_tile ~idx ~cycle v train stations loc in
    let player = _update_player_with_data player data active_stations v.params.fiscal_period v.random in
    train, stations, player, ui_msgs
  end

    (* Run every cycle, updating every train's position and speed *)
  let _update_all_trains (v:t) (player:Player.t) =
    (* Log.debug (fun f -> f "update_all_trains"); *)
    let cycle_check, region_div = if Region.is_us v.params.region then 16, 1 else 8, 2 in
    let cycle_bit = 1 lsl (v.params.cycle mod 12) in
    let cycle = v.params.cycle in
    (* TODO: We update the high priority trains before the low priority *)
    (* Trains are in a vector, updated in-place *)
    let stations, player, ui_msgs =
      Trainmap.fold_mapi_in_place (fun idx (stations, player, ui_msg_acc) train ->
        let train, stations, player, ui_msgs = 
          _update_train v idx train stations player ~cycle ~cycle_check ~cycle_bit ~region_div
        in
        (stations, player, ui_msgs @ ui_msg_acc), train)
        player.trains
        ~init:(v.stations, player, [])
    in
    player.trains, stations, player, ui_msgs
end

let _try_to_create_priority_shipment ?(force=false) (player:Player.t) stations params random =
  (* Try to create a priority shipment:
     TODO: add condition: only after some track exists
     TODO: for all players? check if AI *)
  match player.priority with
  | None ->
      begin match Priority_shipment.try_to_create random stations params.Params.cycle ~force with
      | Some (stations, priority) ->
          let player = Player.set_priority (Some priority) player in
          let msgs = [UIM.PriorityShipmentCreated{player=C.player; shipment=priority}] in
          stations, player, msgs
      | None -> stations, player, []
      end
  | _ -> stations, player, []

let _cancel_expired_priority_shipments ?(force=false) players stations params =
  (* Try to cancel and create corresponding messages *)
  (* Cancel priority and let us know which players' were canceled *)
  let cancel_players =
    Owner.Map.fold (fun idx player acc ->
      if Player.has_priority player &&
        (Player.check_cancel_priority_shipment player params || force)
      then idx::acc else acc)
      players
      []
  in
  let clear_player_and_train_priority_shipments players player_list =
    List.fold_left (fun acc idx ->
      Player.update acc idx @@ fun player ->
        let trains = Trainmap.clear_priority_shipment player.Player.trains in
        [%up {player with trains; priority=None}])
      players
      player_list
  in
  let players = clear_player_and_train_priority_shipments players cancel_players in
  if List.is_empty cancel_players then
    players, stations, []
  else
    let stations = Station_map.clear_priority_shipment_for_all stations ~players:cancel_players in
    let ui_msgs = List.map (fun i -> UIM.PriorityShipmentCanceled{player=i}) cancel_players in
    players, stations, ui_msgs
    
let _check_priority_delivery players stations params =
  (* Check if a priority shipment has been delivered *)
  (* Check for priority delivery completion *)
  let deliver_players =
    Owner.Map.fold (fun idx player acc ->
      if Player.has_priority player && Player.check_priority_delivery player stations
      then idx::acc else acc)
    players
    []
  in
  let players, ui_msgs =
    List.fold_map (fun players idx ->
      let player = Player.get idx players in
      let priority = Player.get_priority player |> Option.get_exn_or "Problem with priority" in
      let bonus = Priority_shipment.compute_bonus priority params in
      let trains = Trainmap.clear_priority_shipment player.trains in
      let player = {player with trains; priority=None}
        |> Player.earn `Other bonus in
      let players = Player.set idx player players in
      let ui_msg = UIM.PriorityShipmentDelivered {player=idx; shipment=priority; bonus} in
      players, ui_msg)
    players
    deliver_players
  in
  if List.is_empty deliver_players then players, stations, ui_msgs
  else
    let stations = Station_map.clear_priority_shipment_for_all stations ~players:deliver_players in
    players, stations, ui_msgs

let _try_to_develop_tiles v (player:Player.t) =
  let age = v.params.year - v.params.year_start in
  (* Originally & with 0x8
     We're already filtering with mod 8 = 0, so this is effectively mod 16 after 25 years
   *)
  if age < 25 || v.params.cycle mod 16 >= 8 then
      let two_devs = Region.is_us v.params.region && age < 40 in
      let dev_state =
      Tile_develop.develop_tiles ~two_devs v.params ~random:v.random ~tilemap:v.map 
        ~active_station:player.active_station ~cities:v.cities
        ~cities_to_ai:v.ai.ai_of_city v.dev_state
      in
      (* Clear active station *)
      dev_state, None
  else
    v.dev_state, player.active_station

let _update_station_supply_demand player_idx stations map params =
  if params.Params.cycle mod C.Cycles.station_supply_demand = 0 then (
    let difficulty = params.options.difficulty in
    let ui_msgs =
      Station_map.fold 
        (fun station old_msgs ->
          Station.check_rate_war_lose_supplies station ~difficulty;
          let msgs = Station.update_supply_demand map params station in
          Station.lose_supplies station;
          let msgs =
            List.map (fun (good, add) ->
              UIM.DemandChanged {player=player_idx; x=fst station.loc; y=snd station.loc; good; add})
              msgs
          in
          msgs @ old_msgs)
      stations
      ~init:[]
    in
    stations, ui_msgs
  ) else
    stations, []

(** Most time-based work happens here **)
let handle_cycle v =
  let time_step () =
    v.params.cycle <- v.params.cycle + 1;

    (* TODO: make player logic work for all human players *)

    let player_idx = C.player in

    let player1 = Player.get player_idx v.players in

    let trains, stations, player, tr_msgs = Train_update._update_all_trains v player1 in

    (* TODO: ai_routines, events, climate update *)
    let player =
      if v.params.cycle mod C.Cycles.periodic_maintenance = 0 then
        if ((v.params.cycle / C.Cycles.periodic_maintenance) mod 2) = 0 then
          Player.pay_station_maintenance v.stations player
        else
          Player.pay_train_maintenance player
      else
        player
    in

    let stations, player, dev_state, active_station, pr_msgs =
      if v.params.cycle mod C.Cycles.rare_bgnd_events = 0 then
        let stations, player, pr_msgs = _try_to_create_priority_shipment player stations v.params v.random in
        let dev_state, active_station = _try_to_develop_tiles v player in
        let player = Player.track_maintenance_random_spot v.track v.random player in
        stations, player, dev_state, active_station, pr_msgs
      else
        stations, player, v.dev_state, player.active_station, []
    in

    let stations, sd_msgs = _update_station_supply_demand C.player stations v.map v.params in

    [%upf player.active_station <- active_station];
    [%upf player.trains <- trains];
    [%upf v.stations <- stations];
    [%upf v.dev_state <- dev_state];

    let player, br_msgs =
      (* Check broker *)
      if Player.has_broker_timer player then
        let player, send_msg = Player.incr_broker_timer player in
        player,
          if send_msg then [(UIM.OpenStockBroker{player=C.player})] else []
      else player, []
    in

    let players, stations = v.players, v.stations in

    let players = if player =!= player1 then Player.set player_idx player players else players in

    (* TODO: only this part deals with all players for now *)

    (* Cancel any expired priority shipments *)
    let players, stations, cp_msgs = _cancel_expired_priority_shipments players stations v.params in

    (* Check if we delivered priority deliveries *)
    let players, stations, del_msgs = _check_priority_delivery players stations v.params in

    [%upf v.stations <- stations];
    [%upf v.players <- players];

    let ui_msgs = del_msgs @ cp_msgs @ br_msgs @ sd_msgs @ pr_msgs @ tr_msgs in

    (* adjust time *)
    v.params.time <- v.params.time + 1;
    let v = 
      if v.params.time >= Constants.year_ticks then
        (* TODO: time should reset on fin period, not year *)
        {v with params={v.params with year=v.params.year + 1; time=0}}
      else v
    in
    v, ui_msgs
  in
  if not v.pause then
    time_step ()
  else v, []

