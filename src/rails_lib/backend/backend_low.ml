
open Containers
open Backend_d
open Utils.Infix

let src = Logs.Src.create "backend_low" ~doc:"Backend_low"
module Log = (val Logs.src_log src: Logs.LOG)

module TS = Trackmap.Search
module G = Track_graph
module C = Constants

(* Low-level backend module. Deals with multiple modules at a time *)

module Segments = struct

  (* When we build a station, we create new station segments on both ends of the station *)
  (* TODO: for upgrade, need to compare before and after *)
  let build_station_get_segments graph stations segments trackmap x y after =
    let ixns = match after with
      | TS.Station ixns -> ixns
      | _ -> assert false
    in
    let dir_segs =
      List.filter_map (fun ixn ->
        let station =
          let exclude_dir = Dir.opposite ixn.TS.dir in
          Track_graph.connected_stations_dirs ~exclude_dir graph stations (x,y)
          |> Iter.head
        in
        match station with
        | Some ((x,y), station_dir) ->
            Log.debug (fun f -> f "Segments: found existing station at (%d,%d)" x y);
            let station = Station_map.get_exn stations x y in
            let seg = Station.get_segment station station_dir in
            Some (ixn.search_dir, seg)
        | _ -> None)
      ixns
    in
    (* Fill in with new segments as needed *)
    match dir_segs with
    | [] ->
        let track = Trackmap.get_exn trackmap x y in
        (* Assert only 2 dirs *)
        Dir.Set.to_list track.dirs 
        |> List.map (fun dir -> dir, Segment.Map.new_id segments)
    | [(dir, _)] as x -> 
        (Dir.opposite dir, Segment.Map.new_id segments)::x
    | _ -> dir_segs

  (* We only care about connecting to a new piece of track that could lead
  to a station. ixns and stations are the same for this
  *)
  let build_track_join_segments graph station_map segments before after =
    let join_ixns = match before, after with
      (* Add an attached ixn: make the two have the same segment *)
      | TS.Track [_], TS.Track [ixn2; ixn3] -> Some (ixn2, ixn3)
      (* Add an ixn to a 2-ixn. Make them all have same segment *)
      | Track (ixn1::_ as l1), Ixn l2 ->
          begin match Utils.find_mismatch ~eq:TS.equal_ixn ~left:l2 ~right:l1 with
          | Some ixn2 -> Some (ixn1, ixn2)
          | None -> None
          end
      | _ -> None
    in
    match join_ixns with
    | None -> ()
    | Some (ixn1, ixn2) ->
        let ixn1 = (ixn1.x, ixn1.y) in
        let ixn2 = (ixn2.x, ixn2.y) in
        let (x1, y1), dir1 =
          Track_graph.connected_stations_dirs graph station_map ixn1 ~exclude_ixns:[ixn2]
          |> Iter.head_exn
        in
        let station1 = Station_map.get_exn station_map x1 y1 in
        let seg1 = Station.get_segment station1 dir1 in
        let stations =
          Track_graph.connected_stations_dirs graph station_map ixn2 ~exclude_ixns:[ixn1]
          |> Iter.to_list
        in
        let (x2, y2), dir2 = List.hd stations in
        let station2 = Station_map.get_exn station_map x2 y2 in
        let seg2 = Station.get_segment station2 dir2 in
        (* Assign seg1 to all connected stations that had seg2 *)
        List.iter (fun ((x, y), _) ->
            Station_map.update station_map x y @@
              (Option.map (fun station -> Station.modify_segment station seg2 seg1)))
          stations;
        (* Update segment map *)
        Segment.Map.merge segments seg1 seg2;
        ()

    (* Removing a piece of track can split a segment. Unfortunately we can't
       keep track of the segment's semaphore value unless we scan the whole segment for
       trains.
       We're too lazy to do that so we'll just set all segment values to 0.
       NOTE: This can cause train crashes. Implement with mapping to trains.
     *)
  let remove_track_split_segment graph station_map segments (before:TS.scan) (after:TS.scan) =
    let separate_pair = match before, after with
      (* Disconnecting a track leading to 2 ixns *)
      | Track [ixn1; ixn2], Track [_] -> Some(ixn1, ixn2)
      (* Disconnecting an ixn *)
      | Ixn l1, Track ((ixn2::_) as l2) ->
          begin match Utils.find_mismatch ~eq:TS.equal_ixn ~left:l1 ~right:l2 with
          | Some ixn1 -> Some (ixn1, ixn2)
          | None -> assert false
          end
      (* Removing a station *)
      | Station [ixn1; ixn2], (Track _ | NoResult) -> Some (ixn1, ixn2)
      | _ -> None
    in
    match separate_pair with
    | None -> ()
    | Some (ixn1, ixn2) ->
        let ixn1 = (ixn1.x, ixn1.y) in
        let ixn2 = (ixn2.x, ixn2.y) in
        let (x1, y1), dir1 =
          Track_graph.connected_stations_dirs graph station_map ixn1 |> Iter.head_exn
        in
        let station1 = Station_map.get_exn station_map x1 y1 in
        let seg1 = Station.get_segment station1 dir1 in
        (* Set value of segment to 0 *)
        Segment.Map.reset segments seg1;
        (* Create a new segment for the split segment *)
        let seg2 = Segment.Map.new_id segments in
        let stations = Track_graph.connected_stations_dirs graph station_map ixn2 in
        (* Assign seg2 to these stations *)
        Iter.iter (fun ((x, y), _) ->
            Station_map.update station_map x y @@
              (Option.map (fun station -> Station.modify_segment station seg1 seg2)))
          stations;
        ()

end

module Graph = struct
  open TS
  (* Routines to handle building/tearing down of track graph *)

  let handle_build_station graph ~x ~y scan1 scan2 =
    (* We just don't add stations until they've been hooked up *)
    let add_to_edge ixn1 _ ixn3 ixn4 =
      graph
      |> G.remove_segment ~x:ixn1.x ~y:ixn1.y ~dir:ixn1.dir
      |> G.add_segment ~xyd1:(ixn3.x,ixn3.y,ixn3.dir) ~xyd2:(x,y,ixn3.search_dir) ~dist:ixn3.dist
      |> G.add_segment ~xyd1:(ixn4.x,ixn4.y,ixn4.dir) ~xyd2:(x,y,ixn4.search_dir) ~dist:ixn4.dist
    in
    match scan1, scan2 with
      (* Unfinished edge. Connect a station here.
          x---       ->    x---s *)
    | Track [ixn1], Station [ixn2] when TS.(equal_ixn ixn1 ixn2) ->
        G.add_segment ~xyd1:(ixn2.x,ixn2.y,ixn2.dir) ~xyd2:(x,y,ixn2.search_dir) ~dist:ixn2.dist graph

    (* Edge. Add a station.
      x-------x  ->    x---s---x
      Remove the edge and rebuild it to the new station.
    *)
    | Track [ixn1; ixn2], Station [ixn3; ixn4]
        when TS.(equal_ixn ixn1 ixn3 && equal_ixn ixn2 ixn4) ->
        add_to_edge ixn1 ixn2 ixn3 ixn4
    | Track [ixn1; ixn2], Station [ixn4; ixn3]
        when TS.(equal_ixn ixn1 ixn3 && equal_ixn ixn2 ixn4) ->
        add_to_edge ixn1 ixn2 ixn3 ixn4
    | _, _ -> graph

  (* Handle simple building of track graph-wise *)
  let handle_build_track graph scan1 scan2 =
    match scan1, scan2 with
      | Track [ixn1], Track [ixn2; ixn3]
          when TS.(equal_ixn ixn1 ixn2 || equal_ixn ixn1 ixn3) ->
          (* Only case: unfinished edge. Connect an intersection.
              x---       ->    x---x *)
          G.add_segment ~xyd1:(ixn2.x,ixn2.y,ixn2.dir) ~xyd2:(ixn3.x,ixn3.y,ixn3.dir)
                        ~dist:(ixn2.dist+ixn3.dist) graph
      | _ -> graph

  (* Handle graph management for building track.
      Complicated because we can have ixns everywhere. 
      TODO: check this for Station *)
    
  let handle_build_track_complex graph ~x ~y scan1 scan2 =
    match scan1, scan2 with
        (* Unfinished edge. Connect an intersection.
          x---       ->    x---x *)
      | Track [ixn1], Track [ixn2; ixn3]
          when TS.(equal_ixn ixn1 ixn2 || equal_ixn ixn1 ixn3) ->
            G.add_segment ~xyd1:(ixn2.x,ixn2.y,ixn2.dir)
                          ~xyd2:(ixn3.x,ixn3.y,ixn3.dir)
                          ~dist:(ixn2.dist+ixn3.dist)
                          graph
        (* Unfinished edge. Create an intersection.
          x---       ->    x--+ *)
      | Track [ixn1], Ixn [ixn2] when TS.equal_ixn ixn1 ixn2 ->
          G.add_segment ~xyd1:(ixn2.x,ixn2.y,ixn2.dir)
                        ~xyd2:(x,y,ixn2.search_dir)
                        ~dist:ixn2.dist
                        graph
        (* Regular edge. We add an intersection in the middle.
          x-----x    ->    x--+--x *)
      | Track [ixn1; ixn2], Ixn [ixn3; ixn4]
        when TS.(equal_ixn ixn1 ixn3 && equal_ixn ixn2 ixn4 || equal_ixn ixn1 ixn4 && equal_ixn ixn2 ixn3) ->
          graph
          |> G.remove_segment ~x:ixn1.x ~y:ixn1.y ~dir:ixn1.dir
          |> G.add_segment ~xyd1:(ixn3.x,ixn3.y,ixn3.dir)
                           ~xyd2:(x,y,ixn3.search_dir)
                           ~dist:ixn3.dist
          |> G.add_segment ~xyd1:(ixn4.x,ixn4.y,ixn4.dir)
                           ~xyd2:(x,y,ixn4.search_dir)
                           ~dist:ixn4.dist
                          

        (* Regular edge. We add an intersection in the middle that connects to another
          intersection:
            x                x
            |                |
          x-----x    ->    x--+--x *)
      | Track [ixn1; ixn2], Ixn [ixn3; ixn4; ixn5]
        when TS.((equal_ixn ixn1 ixn3 && (equal_ixn ixn2 ixn4 || equal_ixn ixn2 ixn5))
          || (equal_ixn ixn1 ixn4 && (equal_ixn ixn2 ixn3 || equal_ixn ixn2 ixn5))
          || (equal_ixn ixn1 ixn5 && (equal_ixn ixn2 ixn3 || equal_ixn ixn2 ixn4))) ->
          graph
          |> G.remove_segment ~x:ixn1.x ~y:ixn1.y ~dir:ixn1.dir
          |> G.add_segment ~xyd1:(ixn3.x,ixn3.y,ixn3.dir)
                           ~xyd2:(x,y,ixn3.search_dir)
                           ~dist:ixn3.dist
          |> G.add_segment ~xyd1:(ixn4.x,ixn4.y,ixn4.dir)
                           ~xyd2:(x,y,ixn4.search_dir)
                           ~dist:ixn4.dist
          |> G.add_segment ~xyd1:(ixn5.x,ixn5.y,ixn5.dir)
                           ~xyd2:(x,y,ixn5.search_dir)
                           ~dist:ixn5.dist
      | _ -> graph
        (* All other cases require no graph changes *)

  let handle_remove_track graph ~x ~y scan1 scan2 =
    match scan1, scan2 with
        (* Was edge. Now disconnected
          x---x       ->    x- -x *)
      | Track [ixn1; ixn2], Track [ixn3]
          when TS.(equal_ixn ixn2 ixn3 || equal_ixn ixn1 ixn3) ->
            G.remove_segment ~x:ixn1.x ~y:ixn1.y ~dir:ixn1.dir graph

        (* Was station. Now station gone.
          x---S       ->    x--- *)
      | Station [_], (Track [_] | NoResult) ->
            G.remove_ixn ~x ~y graph

        (* Was ixn. Now deleted.
          x---+       ->    x--- *)
      | Ixn [_], (Track [_] | NoResult) ->
            G.remove_ixn ~x ~y graph

        (* Was connecting station. Now disconnected
          x---S---x   ->    x--- ---x *)
      | Station [_; _], Track[_] ->
            G.remove_ixn ~x ~y graph

        (* Was 2 ixn. Now edge
          x---+---x   ->    x-------x *)

        (* Was 3 ixn. Now edge + disconnected
              x                 x
              |                 |
          x---+---x   ->    x-------x *)
      | Ixn [_; _], Track [ixn3; ixn4]
      | Ixn [_; _; _], Track [ixn3; ixn4] ->
          graph
          |> G.remove_ixn ~x ~y
          |> G.add_segment ~xyd1:(ixn3.x,ixn3.y,ixn3.dir)
                           ~xyd2:(ixn4.x,ixn4.y,ixn4.dir)
                           ~dist:(ixn3.dist + ixn4.dist)
      | _ -> graph
        (* All other cases require no graph changes *)

end

module Train_update = struct

  let _update_train_target_speed (v:t) (train:Train.t) (track:Track.t) ~idx ~cycle ~x ~y ~dir =
    (* Speed factor computation from height delta and turn *)
    let height1 = Tilemap.get_tile_height v.map x y in
    let x2, y2 = Dir.adjust dir x y in
    let track2 = Trackmap.get_exn v.track x2 y2 in
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
    let track = Trackmap.get_exn v.track x y in
    match track.kind with
    | Station _ ->
        let station = Station_map.get_exn v.stations x y in
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
