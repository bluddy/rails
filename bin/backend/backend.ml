open Containers
open Utils.Infix

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
  fiscal_period: [`First | `Second];
  climate: Climate.t;
  players: Player.t array;
  region: Region.t;
  map : Tilemap.t;
  mutable track: Trackmap.t;
  mutable graph: Track_graph.t;
  trains: Trainmap.t;
  cities: Cities.t;
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
    |> List.iter (fun (name,x,y) -> Hashtbl.replace h (y * width + x) name);
    Cities.make h width height
  in
  let track = Trackmap.empty width height in
  let options = B_options.default in
  let stations = Loc_map.create width in
  let players = Array.make num_players Player.default in
  let year = match region with
    | EastUS -> 1830
    | WestUS -> 1866
    | Britain -> 1828
    | Europe -> 1900
  in
  let trains = Trainmap.empty () in
  let graph = Track_graph.make () in
  {
    time=0;
    cycle=0;
    last_tick=0;
    year;
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
    priority=None;
    options;
    ui_msgs = [];
    random;
    seed;

    stats=Stats.default;
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
  match Tilemap.check_build_track v.map ~x ~y ~dir with
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
  let city = find_close_city ~range:100 v x y |> Option.get_exn_or "error" in
  let check_for_first_city () =
    (* first one has engine shop *)
    match
      Loc_map.filter v.stations (Station.has_upgrade ~upgrade:Station.EngineShop)
      |> Iter.head
    with Some _ -> false | None -> true
  in
  let first = check_for_first_city () in
  let station =
    Station.make ~x ~y
      ~year:v.year
      ~name:city
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
  let engine_t = Engine.t_of_make v.region engine in
  (* Temporary solution for getting track dir *)
  let track = Trackmap.get v.track x y |> Option.get_exn in
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

let _update_train_mid_tile ~idx ~cycle (v:t) (train:Train.t) =
  let tile_x, tile_y = train.x / C.tile_w, train.y / C.tile_h in
  
  (* TODO: check for colocated trains (accidents/stop a train) *)

  let track = Trackmap.get_exn v.track tile_x tile_y in
  (* Adjust direction as needed *)
  let dir =
    if track.ixn &&
      Dir.Set.num_adjacent train.dir track.dirs > 1 then
      let dest = Train.get_dest train in
      Track_graph.shortest_path_branch v.graph
        ~ixn:(tile_x, tile_y) ~dir:train.dir ~dest 
        |> Option.get_exn_or "Cannot find route for train" 
    else
      Dir.Set.find_nearest train.dir track.dirs
      |> Option.get_exn_or "Cannot find track for train"
  in

  (* TODO: handle entering station *)

  (* Speed factor computation from height delta and turn *)
  let height1 = Tilemap.get_tile_height v.map tile_x tile_y in
  let tile_x2, tile_y2 = Dir.adjust dir tile_x tile_y in
  let track2 = Trackmap.get_exn v.track tile_x2 tile_y2 in
  let height2 = Tilemap.get_tile_height v.map tile_x2 tile_y2 in
  let d_height = max 0 (height2 - height1) in
  let d_height = if Dir.is_diagonal dir then d_height else d_height * 3/2 in
  let height_factor = match track.kind, track2.kind with
    | Bridge _ , _ -> 0
    | _, Tunnel -> 0
    | _ -> d_height
  in
  let turn_factor = Dir.diff dir train.dir in
  let speed_factor = (height_factor * height_factor / 144) + turn_factor in
  train.dir <- dir;
  train.pixels_from_midtile <- 0;

  (* History lets us reuse the engine orientation for the cars as they cross *)
  Train.History.add train.history train.x train.y train.dir speed_factor;

  (* Compute and set target speed *)
  begin match Tilemap.get_tile v.map tile_x tile_y with
  | Ocean _ | Harbor _ ->
      Train.set_target_speed train 1;
      Train.set_speed train 1;
  | _ ->
      let target_speed = Train.compute_target_speed train ~idx ~cycle in
      Train.set_target_speed train target_speed;
  end;
  (* Bookkeeping *)
  let dist = if Dir.is_diagonal dir then 2 else 3 in
  Train.add_dist_traveled train dist v.fiscal_period;
  v.stats.dist_traveled <- v.stats.dist_traveled + dist;
  ()

  (* Run every cycle, updating every train's position and speed *)
let _update_all_trains (v:t) =
  let cycle_check, region_div = if Region.is_us v.region then 16, 1 else 8, 2 in
  let cycle_bit = 1 lsl ((v.cycle / 16) mod 12) in

  (* We update the high priority trains more than the low priority *)
  Iter.iter (fun max_priority ->
    Trainmap.iteri (fun idx (train:Train.t) ->
      let priority = (Goods.freight_to_enum train.freight) * 3 - (Train.train_type_to_enum train._type) + 2 in
      if priority <= max_priority &&
         train.wait_time = 0 || train.speed > 0 then begin

        Train.update_speed train ~cycle:v.cycle ~cycle_check ~cycle_bit;

        (* TODO: fiscal period update stuff *)

        (* Train update loop *)
        let rec train_update_loop speed_bound =
          if speed_bound >= train.speed then ()
          else
            let speed =
              if Dir.is_diagonal train.dir then ((train.speed * 2) + 1) / 3
              else train.speed
            in
            let update_val =
              if speed > 12 then 
                if speed_bound = 0 then 12 else speed - 12
              else speed
            in
            (* BUGFIX: original code allowed sampling from random memory *)
            let update_val = update_val / region_div |> min Train.update_array_length in
            if Train.update_cycle_array.(update_val) land cycle_bit <> 0 then (
              let mid_tile_check () =
                (train.x mod C.tile_w) = C.tile_w / 2 &&
                (train.y mod C.tile_h) = C.tile_h / 2
              in
              if mid_tile_check () then (
                 _update_train_mid_tile ~idx ~cycle:v.cycle v train;
              );
              (* Always advance train by single pixel *)
              let dx, dy = Dir.to_offsets train.dir in
              train.x <- train.x + dx;
              train.y <- train.y + dy;
              train.pixels_from_midtile <- succ train.pixels_from_midtile;
            );
            train_update_loop (speed_bound + 12)
        in
        train_update_loop 0;
       end)
    v.trains) @@
  Iter.(0 -- 23)

  (** Most time-based work happens here **)
let _handle_cycle v =
  v.cycle <- v.cycle + 1;

  _update_all_trains v;

  (* TODO: ai_routines *)

  let demand_msgs =
    if v.cycle mod cycles_station_supply_demand = 0 then (
      Printf.printf "_handle_cycle%!\n";
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
      ~init:[]
    )
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

let handle_tick v cur_time =
  (* TODO: handle pausing *)
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
  type stop = [`Stop of int | `Priority]
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

  let run backend = function
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


