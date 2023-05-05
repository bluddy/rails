open Containers
open Utils.Infix

let src = Logs.Src.create "backend" ~doc:"Backend"
module Log = (val Logs.src_log src: Logs.LOG)

module TS = Trackmap.Search
module G = Track_graph
module C = Constants

(* This is the backend. All game-modifying functions go through here *)

(* The actual game (server) state
   Observers can observe data in the backend,
   but actions can only be taken via messages (Backend.Action)
 *)

let tick_ms = 15 (* ms *)
let year_ticks = 2032 (* really 170*12 = 2040 is new year *)
let month_ticks = 170
let num_players = 4


(* Cycle counts to perform some tasks *)
let cycles_periodic_maintenance = 1024
let cycles_priority_delivery = 8
let cycles_background_update = 16
let cycles_ai_update = cycles_background_update
(* In the original game, we do slices of 1/32 stations. No need *)
let cycles_station_supply_demand = cycles_background_update * 32 (* 512 *)
(* Since we don't spread out the supply addition, decay happens in the same cycle *)
let cycles_supply_decay = 512


type ui_msg =
  | TrainBuilt of int
  | DemandChanged of {x: int; y: int; good: Goods.t; add: bool}
  [@@deriving yojson]

type loc = int * int (* x, y *)
  [@@ deriving yojson]

type t = {
  mutable last_tick: int; (* last time we updated a cycle *)
  mutable cycle: int; (* counter used for all sorts of per-tick updates *)
  mutable time: int;  (* In-game time, resets at end of year *)
  mutable year: int;
  year_start: int;
  fiscal_period: [`First | `Second];
  climate: Climate.t;
  west_us_route_done: bool;
  players: Player.t array;
  region: Region.t;
  map : Tilemap.t;
  mutable track: Trackmap.t;
  mutable graph: Track_graph.t;
  trains: Trainmap.t;
  cities: Cities.t;
  engines: Engine.t list;
  mutable stations: Station.t Loc_map.t;
  segments: Segment.Map.t; (* map segments btw stations *)
  priority: (loc * loc * Goods.t) option;  (* priority shipment *)
  stats: Stats.t;
  options: B_options.t;
  mutable ui_msgs: ui_msg list;
  random: Utils.Random.State.t;
  seed: int;
} [@@deriving yojson]

let default region resources ~random ~seed = 
  let map = List.assoc ~eq:(Stdlib.(=)) region resources.Resources.res_maps in
  let map = Tilemap.of_ndarray ~region ~seed map in
  let width, height = Tilemap.get_width map, Tilemap.get_height map in
  let cities =
    let h = Hashtbl.create 100 in
    List.assoc ~eq:(Stdlib.(=)) region resources.res_cities
    |> List.iter (fun (name, x, y) ->
        Hashtbl.replace h (y * width + x) (name, Random.int Station.num_suffix random)
      );
    Cities.make h width height
  in
  let track = Trackmap.empty width height in
  let options = B_options.default in
  let stations = Loc_map.create width in
  let players = Array.make num_players (Player.default options.difficulty) in
  let year = match region with
    | EastUS -> 1830
    | WestUS -> 1866
    | Britain -> 1828
    | Europe -> 1900
  in
  let trains = Trainmap.empty () in
  let graph = Track_graph.make () in
  let engines = Engine.of_region region |> Engine.randomize random in
  {
    time=0;
    cycle=0;
    last_tick=0;
    year;
    year_start=year;
    fiscal_period=`First;
    climate=Moderation;
    players;
    map;
    region;
    cities;
    trains;
    track;
    segments=Segment.Map.make ();
    graph;
    stations;
    engines;
    priority=None;
    options;
    ui_msgs = [];
    random;
    seed;
    stats=Stats.default;
    west_us_route_done=false;
  }

let modify_player v ~player f =
  v.players.(player) <- f (v.players.(player))

let get_speed v = v.options.speed

let _set_speed v speed = {v with options={v.options with speed}}

let map_height v = Tilemap.get_height v.map

let map_width v = Tilemap.get_width v.map

let get_tile v x y = Tilemap.get_tile v.map x y

let get_track v x y = Trackmap.get v.track x y

let get_cities v = Cities.to_list v.cities

let get_station v x y = Loc_map.get v.stations x y

let get_region v = v.region

let get_map v = v.map

let get_tile_height v x y = Tilemap.get_tile_height v.map x y

let iter_cities f v = Cities.iter f v.cities

let find_close_city v x y ~range = Cities.find_close v.cities x y ~range

let check_build_track v ~x ~y ~dir ~player =
  (* First check the tilemap, then the trackmap *)
  match Tilemap.check_build_track v.map ~x ~y ~dir ~difficulty:v.options.difficulty with
  | `Bridge when Trackmap.check_build_stretch v.track ~x ~y ~dir ~player ~length:2 -> `Bridge
  | `Tunnel(length, _) as tun when Trackmap.check_build_stretch v.track ~x ~y ~dir ~player ~length -> tun
  | (`Tunnel(_,g) | `HighGrade g) when Trackmap.check_build_track v.track ~x ~y ~dir ~player -> `HighGrade g
  | (`Ok | `Ferry) as ret when Trackmap.check_build_track v.track ~x ~y ~dir ~player -> ret
  | _ -> `Illegal

let get_money v ~player = Player.get_money v.players.(player)


let _build_tunnel v ~x ~y ~dir ~player ~length =
  let before = TS.scan v.track ~x ~y ~player in
  let track = Trackmap.build_tunnel v.track ~x ~y ~dir ~player ~length in
  let after = TS.scan track ~x ~y ~player in
  let graph = Backend_low.Graph.handle_build_track v.graph before after in
  Backend_low.Segments.build_track_join_segments graph v.stations v.segments before after;
  modify_player v ~player (Player.add_track ~length);
  if v.graph =!= graph then v.graph <- graph;
  if v.track =!= track then v.track <- track;
  v

let check_build_station v ~x ~y ~player station_type =
  match Trackmap.check_build_station v.track ~x ~y ~player station_type with
  | `Ok -> Tilemap.check_build_station v.map ~x ~y
  | x -> x

let _build_station v ~x ~y station_type ~player =
  let before = TS.scan v.track ~x ~y ~player in
  let track, build_new_track = Trackmap.build_station v.track ~x ~y station_type in
  let after = TS.scan track ~x ~y ~player in
  let graph = Backend_low.Graph.handle_build_station v.graph ~x ~y before after in
  let dir_segments = Backend_low.Segments.build_station_get_segments graph v.stations v.segments track x y after in
  let station = match station_type with
  | `SignalTower ->
    Station.make_signaltower ~x ~y ~year:v.year ~player ~segments:dir_segments
  | _ ->
    let city_xy = find_close_city ~range:100 v x y |> Option.get_exn_or "error" in
    let check_for_first_city () =
      (* first one has engine shop *)
      match
        Loc_map.filter v.stations 
          (fun v -> Station.has_upgrade v Station.EngineShop)
        |> Iter.head
      with Some _ -> false | None -> true
    in
    let first = check_for_first_city () in
    (* Get suffix if needed *)
    let city_name, suffix =
      let (x,y) = city_xy in
      let name, offset = Cities.find_exn v.cities x y in
      let count =
        Loc_map.fold (fun station count ->
          match Station.get_city station with
          | Some (city_x, city_y) ->
            if city_x = x && city_y = y then count + 1
            else count
          | _ -> count)
        v.stations
        ~init:0
      in
      if count = 0 then name, None
      else
        let suffix_n = (offset + count) mod Station.num_suffix in
        name, Station.suffix_of_enum suffix_n
    in
    Station.make ~x ~y
      ~year:v.year
      ~city_xy
      ~suffix
      ~city_name
      ~kind:station_type
      ~player
      ~first
      ~segments:dir_segments
  in
  let stations = Loc_map.add v.stations x y station in
  if build_new_track then (
    modify_player v ~player @@ Player.add_track ~length:1
  );
  (* Initialize supply and demand *)
  let simple_economy =
    not @@ B_options.RealityLevels.mem v.options.reality_levels `ComplexEconomy 
  in
  let climate = v.climate in
  ignore @@ Station.update_supply_demand station v.map ~climate ~simple_economy;

  if v.track =!= track then v.track <- track;
  if v.graph =!= graph then v.graph <- graph;
  if v.stations =!= stations then v.stations <- stations;
  v

let check_build_bridge v ~x ~y ~dir ~player =
  match check_build_track v ~x ~y ~dir ~player with
  | `Bridge -> `Ok
  | _ -> `Illegal

let _build_bridge v ~x ~y ~dir ~player ~kind =
  let before = TS.scan v.track ~x ~y ~player in
  let track = Trackmap.build_bridge v.track ~x ~y ~dir ~player ~kind in
  modify_player v ~player (Player.add_track ~length:2);
  let after = TS.scan track ~x ~y ~player in
  let graph = Backend_low.Graph.handle_build_track v.graph before after in
  Backend_low.Segments.build_track_join_segments graph v.stations v.segments before after;
  if v.track =!= track then v.track <- track;
  if v.graph =!= graph then v.graph <- graph;
  v

let _build_track (v:t) ~x ~y ~dir ~player =
  (* Can either create a new edge or a new node (ixn) *)
  let before = TS.scan v.track ~x ~y ~player in
  let track = Trackmap.build_track v.track ~x ~y ~dir ~player in
  modify_player v ~player (Player.add_track ~length:1);
  let after = TS.scan track ~x ~y ~player in
  let graph = Backend_low.Graph.handle_build_track_complex v.graph ~x ~y before after in
  Backend_low.Segments.build_track_join_segments graph v.stations v.segments before after;
  if v.track =!= track then v.track <- track;
  if v.graph =!= graph then v.graph <- graph;
  v

let _build_ferry v ~x ~y ~dir ~player =
  let before = TS.scan v.track ~x ~y ~player in
  let tile1 = get_tile v x y in
  let dx, dy = Dir.to_offsets dir in
  let tile2 = get_tile v (x+dx) (y+dy) in
  let kind1, kind2 = match tile1, tile2 with
    | Tile.Ocean _ , Ocean _ -> Track.Ferry, Track.Ferry
    | Ocean _, _ -> Ferry, Track
    | _, Ocean _ -> Track, Ferry
    | _ -> assert false
  in
  let track = Trackmap.build_track v.track ~x ~y ~dir ~player ~kind1 ~kind2 in
  modify_player v ~player (Player.add_track ~length:1);
  let after = TS.scan track ~x ~y ~player in
  let graph = Backend_low.Graph.handle_build_track v.graph before after in
  Backend_low.Segments.build_track_join_segments graph v.stations v.segments before after;
  if v.track =!= track then v.track <- track;
  if v.graph =!= graph then v.graph <- graph;
  v

let check_remove_track v ~x ~y ~dir ~player=
  Trackmap.check_remove_track v.track ~x ~y ~dir ~player

let _remove_track v ~x ~y ~dir ~player =
  let before = TS.scan v.track ~x ~y ~player in
  let track = Trackmap.remove_track v.track ~x ~y ~dir ~player in
  let after = TS.scan track ~x ~y ~player in
  let graph = Backend_low.Graph.handle_remove_track v.graph ~x ~y before after in
  Backend_low.Segments.remove_track_split_segment graph v.stations v.segments before after;
  modify_player v ~player (Player.add_track ~length:(-1));
  if v.track =!= track then v.track <- track;
  if v.graph =!= graph then v.graph <- graph;
  v

let _improve_station v ~x ~y ~player ~upgrade =
  let stations = 
    match get_station v x y with
    | Some station ->
        let station = Station.add_upgrade station upgrade player in
        Loc_map.add v.stations x y station
    | None -> v.stations
  in
  if v.stations =!= stations then v.stations <- stations;
  v

let _build_train v ((x, y) as station) engine cars other_station =
  let engine_t = Engine.t_of_make v.engines engine in
  (* TODO: Temporary solution for getting track dir *)
  let track = Trackmap.get v.track x y |> Option.get_exn_or "trackmap" in
  let dir, _ = Dir.Set.pop track.dirs in
  let train = Train.make station engine_t cars other_station ~dir in
  let trains = Trainmap.add v.trains train in
  let msg = TrainBuilt (Trainmap.size v.trains - 1) in
  v.ui_msgs <- msg::v.ui_msgs;
  if trains === v.trains then v else {v with trains}

let _remove_stop_car v ~train ~stop ~car =
  let trains =
    Trainmap.update v.trains train
      (fun train -> Train.remove_stop_car train stop car)
  in
  if trains =!= v.trains then {v with trains} else v

let check_stop_station v ~train ~stop ~station =
  let train = Trainmap.get v.trains train in
  Train.check_stop_station train stop station

let _set_stop_station v ~train ~stop ~station =
  let trains =
    Trainmap.update v.trains train
      (fun train -> Train.set_stop_station train stop station)
  in
  if trains =!= v.trains then {v with trains} else v

let _remove_stop v ~train ~stop =
  let trains =
    Trainmap.update v.trains train
      (fun train -> Train.remove_stop train stop)
  in
  if trains =!= v.trains then {v with trains} else v

let _add_stop_car v ~train ~stop ~car =
  let trains =
    Trainmap.update v.trains train
      (fun train -> Train.add_stop_car train stop car)
  in
  if trains =!= v.trains then {v with trains} else v

let _remove_all_stop_cars v ~train ~stop =
  let trains =
    Trainmap.update v.trains train
      (fun train -> Train.remove_all_stop_cars train stop)
  in
  if trains =!= v.trains then {v with trains} else v

let get_num_trains v = Trainmap.size v.trains

let get_train v idx = Trainmap.get v.trains idx
  
let trackmap_iter v f = Trackmap.iter v.track f

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

  (* History lets us reuse the engine orientation for the cars as they cross *)
  Train.History.add train.history train.x train.y train.dir speed_factor;

  (* Compute and set target speed *)
  let target_speed, speed =
    match Tilemap.get_tile v.map x y with
    | Ocean _ | Harbor _ -> 1, 1
    | _ -> Train.compute_target_speed train ~idx ~cycle, train.speed
  in
  train.pixels_from_midtile <- 0;
  if dir =!= train.dir then train.dir <- dir;
  if target_speed =!= train.target_speed then train.target_speed <- target_speed;
  if speed =!= train.speed then train.speed <- speed;
  (* Bookkeeping *)
  let dist = if Dir.is_diagonal dir then 2 else 3 in
  Train.add_dist_traveled train dist v.fiscal_period;
  v.stats.dist_traveled <- v.stats.dist_traveled + dist;
  Train.advance train

let _train_class_stops_at station_info train = 
  let train = Train.train_type_to_enum train.Train._type in
  let station = Station.kind_to_enum station_info.Station.kind in
  station > train

let _train_stops_at (station:Station.t) train =
  match station.info with
  | Some station_info ->
    _train_class_stops_at station_info train &&
    Utils.eq_xy (Train.get_dest train) (station.x, station.y)
  | None -> false

let _train_enter_station (v:t) ((x,y) as loc) (station:Station.t) (train:Train.t) =
  let handle_stop station_info =
    let had_maintenance =
      if Station.can_maintain station then true
      else train.had_maintenance
    in
    (* TODO: deal with priority shipment *)
    let dist_shipped_cargo =
      let dist = Utils.classic_dist loc train.last_station in
      let num_cars = List.length train.cars in
      let total_dist = dist * num_cars in
      Train.add_dist_shipped_cargo train total_dist v.fiscal_period
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
    let car_change_work, car_change_expense, cars, station_supply =
      Train.dump_unused_cars_to_station train station_supply
    in
    let time_for_car_change =
      let multiplier = if Station.has_upgrade station Station.SwitchingYard then 16 else 64 in
      car_change_work * multiplier
    in
    let freight = Train.freight_of_cars cars in
    let time_pickup, cars, station_supply =
      Train.fill_train_from_station cars loc v.cycle station_supply in
    let wait_time = time_for_sold_goods + time_for_car_change + time_pickup in
    let income = (Utils.sum money_from_goods) + other_income + car_change_expense in
    {train with cars; had_maintenance; wait_time; freight}, income, []
  in
  match station.info with
  | Some station_info when _train_stops_at station train ->
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

let _update_train_mid_tile ~idx ~cycle (v:t) (train:Train.t) =
  (* All major computation happens mid-tile *)
  let (x,y) as loc = train.x / C.tile_w, train.y / C.tile_h in
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
      let station = Loc_map.get_exn v.stations x y in
      let enter train =
        begin match train.Train.segment with
        | Some segment ->
            (* exit segment *)
            Segment.Map.decr_train v.segments segment
        | _ -> ()
        end;
        let last_station, priority, stop, train, income, ui_msgs =
          if Station.is_proper_station station then (
            let train, income, ui_msgs = _train_enter_station v loc station train in
            let priority, stop = Train.check_increment_stop train loc in
            loc, priority, stop, train, income, ui_msgs
          ) else (
            train.last_station, train.priority, train.stop, train, 0, []
          )
        in
        {train with segment=None; last_station; priority; stop; station_state=`Entered}
      in
      let exit train =
        (* Can we leave? *)
        if train.Train.wait_time = 0 && not train.stop_at_station then (
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
            _update_train_target_speed v train track ~idx ~cycle ~x ~y ~dir
          in
          {train with segment=Some segment; station_state=`Traveling}) 
        else
          train
      in
      begin match train.station_state with
      | `Traveling ->
          let train = enter train in
          exit train 
      | `Entered when train.wait_time > 0 ->
          train
      | `Entered ->
          exit train
      end

  | Track when track.ixn && Dir.Set.num_adjacent train.dir track.dirs > 1 ->
      let dir =
        let dest = Train.get_dest train in
        Track_graph.shortest_path_branch v.graph
          ~ixn:loc ~cur_dir:train.dir ~dest 
          |> Option.get_exn_or "Cannot find route for train" 
      in
      _update_train_target_speed v train track ~idx ~cycle ~x ~y ~dir

  | _ -> (* All other track and non-deicsion ixns *)
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

  (* TODO: We update the high priority trains before the low priority *)
  let update_train idx (train:Train.t) =
    (* let priority = (Goods.freight_to_enum train.freight) * 3 - (Train.train_type_to_enum train._type) + 2 in *)
    if train.wait_time = 0 || train.speed > 0 then (
      let train = Train.update_speed train ~cycle:v.cycle ~cycle_check ~cycle_bit in
      (* TODO: fiscal period update stuff *)
      let rec train_update_loop train speed_bound =
        if speed_bound >= train.Train.speed then
          train
        else (
          let speed =
            if train.speed > 1 && Dir.is_diagonal train.dir then
              (train.speed * 2 + 1) / 3
            else
              train.speed
          in
          let update_val =
            if speed > 12 then 
              if speed_bound = 0 then 12 else speed - 12
            else speed
          in
          (* BUGFIX: original code allowed sampling from random memory *)
          let update_val =
            update_val / region_div |> min Train.update_array_length
          in
          (* Log.debug (fun f -> f "Update val %d, cycle_bit %d" update_val cycle_bit); *)
          let train =
            if (Train.update_cycle_array.(update_val) land cycle_bit) <> 0 then (
              (* Log.debug (fun f -> f "Pass test. Update val %d, cycle_bit %d" update_val cycle_bit); *)
              let is_mid_tile =
                (train.x mod C.tile_w) = C.tile_w / 2 &&
                (train.y mod C.tile_h) = C.tile_h / 2
              in
              if is_mid_tile then
                 _update_train_mid_tile ~idx ~cycle:v.cycle v train
              else
                Train.advance train)
            else
              train
          in
          train_update_loop train (speed_bound + 12)
        )
      in
      train_update_loop train 0)
    else ( (* wait time > 0 *)
      Train.decr_wait_time train;
      Train.set_target_speed train 4;
      train
    )
  in
  Trainmap.mapi_in_place update_train v.trains

  (** Most time-based work happens here **)
let _handle_cycle v =
  v.cycle <- v.cycle + 1;
  _update_all_trains v;
  (* TODO: ai_routines *)
  let demand_msgs =
    if v.cycle mod cycles_station_supply_demand = 0 then (
      (* Printf.printf "_handle_cycle%!\n"; *)
      let difficulty = v.options.difficulty in
      let climate = v.climate in
      let simple_economy =
        not @@ B_options.RealityLevels.mem v.options.reality_levels `ComplexEconomy 
      in
      Loc_map.fold 
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
      {v with year=v.year + 1}
    else v
  in
  v, demand_msgs

let reset_tick v =
  v.last_tick <- 0

let handle_tick v cur_time =
  let delay_mult = B_options.delay_mult_of_speed v.options.speed in
  let tick_delta = delay_mult * tick_ms in
  let new_time = v.last_tick + tick_delta in
  let ui_msgs = v.ui_msgs in
  v.ui_msgs <- [];

  if cur_time >= new_time then (
    v.last_tick <- cur_time;
    let v, misc_msgs = _handle_cycle v in
    v, ui_msgs @ misc_msgs
  )
  else
    v, ui_msgs

let _month_of_time time = (time / month_ticks) mod 12

let get_date v = _month_of_time v.time, v.year

module Action = struct
  type stop = [`Stop of int | `Priority] [@@deriving show]
  type t =
    | NoAction
    | BuildTrack of Utils.msg
    | BuildFerry of Utils.msg
    | BuildStation of {x: int; y: int; kind: Station.kind; player: int}
    | BuildBridge of Utils.msg * Bridge.t
    | BuildTunnel of Utils.msg * int (* length *)
    | RemoveTrack of Utils.msg
    | ImproveStation of {x:int; y:int; player: int; upgrade: Station.upgrade}
    | SetSpeed of B_options.speed
    | BuildTrain of {engine: Engine.make; cars: Goods.t list; station: int * int; other_station: (int * int) option} 
    | SetStopStation of {train: int; stop: stop; station: int * int}
    | RemoveStop of {train: int; stop: stop}
    | AddStopCar of {train: int; stop: stop; car: Goods.t}
    | RemoveStopCar of {train: int; stop: stop; car: int}
    | RemoveAllStopCars of {train: int; stop: stop}
    [@@deriving show]

  let has_action = function NoAction -> false | _ -> true

  let run backend msg =
    if has_action msg then Log.debug (fun f -> f "Received msg %s" (show msg));
    match msg with
    | BuildTrack {x; y; dir; player} ->
        _build_track backend ~x ~y ~dir ~player
    | BuildFerry {x; y; dir; player} ->
        _build_ferry backend ~x ~y ~dir ~player
    | BuildStation {x; y; kind; player} ->
        _build_station backend ~x ~y kind ~player
    | BuildBridge({x; y; dir; player}, kind) ->
        _build_bridge backend ~x ~y ~dir ~kind ~player
    | BuildTunnel({x; y; dir; player}, length) ->
        _build_tunnel backend ~x ~y ~dir ~player ~length
    | RemoveTrack {x; y; dir; player} ->
        _remove_track backend ~x ~y ~dir ~player
    | ImproveStation {x; y; player; upgrade} ->
        _improve_station backend ~x ~y ~player ~upgrade
    | SetSpeed speed ->
        _set_speed backend speed
    | BuildTrain {engine; cars; station; other_station} ->
        _build_train backend station engine cars other_station
    | RemoveStopCar {train; stop; car} ->
        _remove_stop_car backend ~train ~stop ~car
    | SetStopStation {train; stop; station} ->
        _set_stop_station backend ~train ~stop ~station
    | RemoveStop {train; stop} ->
        _remove_stop backend ~train ~stop
    | RemoveAllStopCars {train; stop} ->
        _remove_all_stop_cars backend ~train ~stop
    | AddStopCar {train; stop; car} ->
        _add_stop_car backend ~train ~stop ~car
    | NoAction -> backend

end


