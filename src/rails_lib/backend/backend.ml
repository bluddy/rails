open Containers
open Utils.Infix
open Backend_d

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

type t = Backend_d.t [@@deriving yojson]
type ui_msg = [%import: Backend_d.ui_msg]

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
  let stations = Station_map.empty in
  let players = Array.make num_players (Player.default options.difficulty) in
  let year = match region with
    | EastUS -> 1830
    | WestUS -> 1866
    | Britain -> 1828
    | Europe -> 1900
  in
  let trains = Trainmap.empty () in
  let graph = Track_graph.make () in
  let engines = Engine.of_region region |> Engine.randomize_year random in
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
    west_us_route_done=false;
    pause=false;
  }

let get_speed v = v.options.speed

let _set_speed v speed = {v with options={v.options with speed}}

let map_height v = Tilemap.get_height v.map

let map_width v = Tilemap.get_width v.map

let get_tile v x y = Tilemap.get_tile v.map x y

let get_track v x y = Trackmap.get v.track x y

let get_cities v = Cities.to_list v.cities

let get_station loc v = Station_map.get loc v.stations

let get_region v = v.region

let get_map v = v.map

let get_tile_height v x y = Tilemap.get_tile_height v.map x y

let iter_cities f v = Cities.iter f v.cities

let find_close_city v x y ~range = Cities.find_close v.cities x y ~range

let _player_pay_for_track v ~x ~y ~len ~dir ~player =
  let base_length = if Dir.is_diagonal dir then 3 else 2 in
  (* includes climate, for one piece of track *)
  let track_expense = (base_length * 2 * ((Climate.to_enum v.climate) + 4)) / 4 in
  let land_expense =
    Tilemap.track_land_expense v.map ~track_expense ~x ~y ~dir ~len
  in
  let open Player in
  update_player v player @@ add_track ~length:(len * base_length);
  update_player v player @@ pay TrackExpense (track_expense * len);
  update_player v player @@ pay LandExpense land_expense;
  ()

let get_money v ~player = Player.get_money v.players.(player)

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
        Station_map.filter 
          (fun v -> Station.has_upgrade v Station.EngineShop)
        v.stations 
        |> Iter.head
      with Some _ -> false | None -> true
    in
    let first = check_for_first_city () in
    (* Get suffix if needed *)
    let city_name, suffix =
      let (x,y) = city_xy in
      let name, offset = Cities.find_exn v.cities x y in
      let count =
        Station_map.fold (fun station count ->
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
  let stations = Station_map.add (x,y) station v.stations in
  if build_new_track then (
    update_player v player @@ Player.add_track ~length:1
  );
  (* Initialize supply and demand *)
  let simple_economy =
    not @@ B_options.RealityLevels.mem v.options.reality_levels `ComplexEconomy 
  in
  let climate = v.climate in
  ignore @@ Station.update_supply_demand station v.map ~climate ~simple_economy;

  update_player v player @@
    Player.pay Player.StationExpense (Station.price_of station_type);

  if v.track =!= track then v.track <- track;
  if v.graph =!= graph then v.graph <- graph;
  if v.stations =!= stations then v.stations <- stations;
  v

let check_build_tunnel v ~x ~y ~dir ~player =
  let check length = Trackmap.check_build_stretch v.track ~x ~y ~dir ~player ~length in
  match Tilemap.check_build_tunnel v.map ~x ~y ~dir with
  | `Tunnel(length, _, _) when not(check length) -> `HitsTrack
  | x -> x

let _build_tunnel v ~x ~y ~dir ~player =
  match check_build_tunnel v ~x ~y ~dir ~player with
  | `Tunnel(length, _, cost) ->
    let before = TS.scan v.track ~x ~y ~player in
    let track = Trackmap.build_tunnel v.track ~x ~y ~dir ~player ~length in
    let after = TS.scan track ~x ~y ~player in
    let graph = Backend_low.Graph.handle_build_track v.graph before after in
    let stations = Backend_low.Segments.build_track_join_segments graph v.stations v.segments before after in
    update_player v player @@ Player.pay Player.TunnelExpense cost;
    if v.stations =!= stations then v.stations <- stations;
    if v.graph =!= graph then v.graph <- graph;
    if v.track =!= track then v.track <- track;
    v
  | _ -> v

let _build_bridge v ~x ~y ~dir ~player ~kind =
  let before = TS.scan v.track ~x ~y ~player in
  let track = Trackmap.build_bridge v.track ~x ~y ~dir ~player ~kind in
  let after = TS.scan track ~x ~y ~player in
  let graph = Backend_low.Graph.handle_build_track v.graph before after in
  let stations = Backend_low.Segments.build_track_join_segments graph v.stations v.segments before after in
  _player_pay_for_track v ~x ~y ~dir ~player ~len:2;
  update_player v player @@ Player.pay Player.TrackExpense (Bridge.price_of kind);
  if v.stations =!= stations then v.stations <- stations;
  if v.graph =!= graph then v.graph <- graph;
  if v.track =!= track then v.track <- track;
  v

let check_build_track v ~x ~y ~dir ~player =
  (* First check the tilemap, then the trackmap *)
  let ret = Tilemap.check_build_track v.map ~x ~y ~dir ~difficulty:v.options.difficulty in
  match ret with
  | `Bridge when Trackmap.check_build_stretch v.track ~x ~y ~dir ~player ~length:2 -> `Bridge
  | `Ok | `Ferry | `Tunnel _ | `HighGrade _ when Trackmap.check_build_track v.track ~x ~y ~dir ~player -> ret
  | _ -> `Illegal

let check_build_bridge v ~x ~y ~dir ~player =
  match check_build_track v ~x ~y ~dir ~player with
  | `Bridge -> `Ok
  | _ -> `Illegal

let check_make_double_track v ~x ~y =
  match Trackmap.get v.track x y with
  | Some ({kind = Track `Single; _} as track) when Track.is_doubleable track -> true
  | Some ({kind = Ferry `Single; _} as track) when Track.is_doubleable track -> true
  | _ -> false

let check_make_single_track v ~x ~y =
  match Trackmap.get v.track x y with
  | Some {kind = Track `Double; _} -> true
  | Some {kind = Ferry `Double; _} -> true
  | _ -> false

let _make_double_track (v:t) ~x ~y =
  if check_make_double_track v ~x ~y then (
    let t = Trackmap.get_exn v.track x y in
    let t = {t with kind=Track `Double} in
    let track = Trackmap.set v.track x y t in
    {v with track}
  ) else v

let _make_single_track (v:t) ~x ~y =
  if check_make_single_track v ~x ~y then (
    let t = Trackmap.get_exn v.track x y in
    let t = match t.kind with
      | Track `Double -> {t with kind=Track `Single}
      | Ferry `Double -> {t with kind=Ferry `Single}
      | _ -> assert false
    in
    let track = Trackmap.set v.track x y t in
    {v with track}
  ) else v
    
let _build_track (v:t) ~x ~y ~dir ~player =
  (* Can either create a new edge or a new node (ixn) *)
  let before = TS.scan v.track ~x ~y ~player in
  let track = Trackmap.build_track v.track ~x ~y ~dir ~player in
  let after = TS.scan track ~x ~y ~player in
  let graph = Backend_low.Graph.handle_build_track_complex v.graph ~x ~y before after in
  let stations = Backend_low.Segments.build_track_join_segments graph v.stations v.segments before after in
  _player_pay_for_track v ~x ~y ~dir ~player ~len:1;
  if v.stations =!= stations then v.stations <- stations;
  if v.graph =!= graph then v.graph <- graph;
  if v.track =!= track then v.track <- track;
  v

let _build_ferry v ~x ~y ~dir ~player =
  let before = TS.scan v.track ~x ~y ~player in
  let tile1 = get_tile v x y in
  let dx, dy = Dir.to_offsets dir in
  let tile2 = get_tile v (x+dx) (y+dy) in
  let kind1, kind2 = match tile1, tile2 with
    | Tile.Ocean _ , Ocean _ -> Track.Ferry `Single, Track.Ferry `Single
    | Ocean _, _ -> Ferry `Single, Track `Single
    | _, Ocean _ -> Track `Single, Ferry `Single
    | _ -> assert false
  in
  let track = Trackmap.build_track v.track ~x ~y ~dir ~player ~kind1 ~kind2 in
  let after = TS.scan track ~x ~y ~player in
  let graph = Backend_low.Graph.handle_build_track v.graph before after in
  let stations = Backend_low.Segments.build_track_join_segments graph v.stations v.segments before after in
  _player_pay_for_track v ~x ~y ~dir ~player ~len:1;
  if v.stations =!= stations then v.stations <- stations;
  if v.graph =!= graph then v.graph <- graph;
  if v.track =!= track then v.track <- track;
  v

let check_remove_track v ~x ~y ~dir ~player=
  Trackmap.check_remove_track v.track ~x ~y ~dir ~player

let _remove_track v ~x ~y ~dir ~player =
  let before = TS.scan v.track ~x ~y ~player in
  let track = Trackmap.remove_track v.track ~x ~y ~dir ~player in
  let after = TS.scan track ~x ~y ~player in
  let graph = Backend_low.Graph.handle_remove_track v.graph ~x ~y before after in
  let stations = Backend_low.Segments.remove_track_split_segment graph v.stations v.segments before after in
  update_player v player (Player.add_track ~length:(-1));
  if v.stations =!= stations then v.stations <- stations;
  if v.track =!= track then v.track <- track;
  if v.graph =!= graph then v.graph <- graph;
  v

let _improve_station v ~x ~y ~player ~upgrade =
  let stations = 
    Loc_map.update (x,y)
      (Option.map @@ fun station -> Station.add_upgrade station upgrade player)
      v.stations
  in
  update_player v player @@
    Player.(pay StationExpense @@ Station.price_of_upgrade upgrade);
  if v.stations =!= stations then v.stations <- stations;
  v

let _build_train v ((x, y) as station) engine cars other_station ~player =
  let engine_t = Engine.t_of_make v.engines engine in
  (* TODO: Temporary solution for getting track dir *)
  let track = Trackmap.get v.track x y |> Option.get_exn_or "trackmap" in
  let dir, _ = Dir.Set.pop track.dirs in
  let train = Train.make station engine_t cars other_station ~dir ~player in
  let trains = Trainmap.add v.trains train in
  update_player v player (Player.pay Player.TrainExpense engine_t.price);
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

let _train_set_type v ~train ~typ =
  let trains =
    Trainmap.update v.trains train (fun train -> Train.set_type train typ)
  in
  if v.trains =!= trains then {v with trains} else v

let _train_replace_engine v ~train ~engine ~player =
  let engine = Engine.t_of_make v.engines engine in
  let trains =
    Trainmap.update v.trains train
      (fun train -> Train.replace_engine train engine)
  in
  update_player v player (Player.pay Player.TrainExpense engine.price);
  if v.trains =!= trains then {v with trains} else v

let _remove_train v idx =
  let train = Trainmap.get v.trains idx in
  (match train.segment with
  | Some segment_id ->
      Segment.Map.decr_train v.segments segment_id
  | _ -> ());
  let trains = Trainmap.delete v.trains idx in
  if trains =!= v.trains then {v with trains} else v

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
    let v, misc_msgs = Backend_low.handle_cycle v in
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
    | Pause
    | Unpause
    | BuildTrack of Utils.msg
    | BuildFerry of Utils.msg
    | BuildStation of {x: int; y: int; kind: Station.kind; player: int}
    | BuildBridge of Utils.msg * Bridge.t
    | BuildTunnel of Utils.msg * int (* length: 3 or 2 * length *)
    | RemoveTrack of Utils.msg
    | DoubleTrack of bool * int * int
    | ImproveStation of {x:int; y:int; player: int; upgrade: Station.upgrade}
    | SetSpeed of B_options.speed
    | BuildTrain of {engine: Engine.make;
                     cars: Goods.t list;
                     station: int * int;
                     other_station: (int * int) option;
                     player: int;
                    } 
    | SetStopStation of {train: int; stop: stop; station: int * int}
    | RemoveStop of {train: int; stop: stop}
    | AddStopCar of {train: int; stop: stop; car: Goods.t}
    | RemoveStopCar of {train: int; stop: stop; car: int}
    | RemoveAllStopCars of {train: int; stop: stop}
    | TrainSetType of {train: int; typ: Train.train_type}
    | RemoveTrain of int
    | TrainReplaceEngine of {train: int; engine: Engine.make}
    [@@deriving show]

  let has_action = function NoAction -> false | _ -> true

  let run backend msgs =
    let run_single backend msg =
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
      | BuildTunnel({x; y; dir; player}, _length) ->
          _build_tunnel backend ~x ~y ~dir ~player
      | RemoveTrack {x; y; dir; player} ->
          _remove_track backend ~x ~y ~dir ~player
      | DoubleTrack (true, x, y) ->
          _make_double_track backend ~x ~y
      | DoubleTrack (false, x, y) ->
          _make_single_track backend ~x ~y
      | ImproveStation {x; y; player; upgrade} ->
          _improve_station backend ~x ~y ~player ~upgrade
      | SetSpeed speed ->
          _set_speed backend speed
      | BuildTrain {engine; cars; station; other_station; player} ->
          _build_train backend station engine cars other_station ~player
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
      | TrainSetType {train; typ} ->
          _train_set_type backend ~train ~typ
      | RemoveTrain idx ->
          _remove_train backend idx
      | TrainReplaceEngine {train; engine} ->
          _train_replace_engine backend ~train ~engine ~player:0
      | Pause -> {backend with pause=true}
      | Unpause -> {backend with pause=false}
      | NoAction -> backend
    in
    List.fold_left run_single backend msgs
end


