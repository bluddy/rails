open Containers
open Sexplib.Std

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
let cycles_station_supply_demand = cycles_background_update * 32
let cycles_supply_decay = 512



module Random = struct
  (* Expand Random to serialize the state *)
  include Random
  module State = struct
    include Random.State
    let t_of_sexp = function
    | Sexplib.Sexp.Atom s ->
        Marshal.from_string s 0
    | _ -> failwith "unexpected sexp"
    let sexp_of_t v =
      Sexplib.Sexp.Atom (Marshal.to_string v [])
    (* let sexp_of_t v = Sexplib.Sexp.Atom "randomstuff" *)
  end
end

type t = {
  mutable last_tick: int; (* last time we updated a cycle *)
  mutable cycle: int; (* counter used for all sorts of per-tick updates *)
  mutable time: int;  (* In-game time, resets at end of year *)
  mutable year: int;
  climate: Climate.t;
  players: Player.t array;
  region: Region.t;
  map : Tilemap.t;
  mutable track: Trackmap.t;
  cities: Cities.t;
  mutable stations: Station_map.t;
  options : B_options.t;
  random: Random.State.t;
  seed: int;
} [@@deriving sexp]

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
  let stations = Station_map.create width in
  let players = Array.make num_players Player.default in
  {
    time=0;
    cycle=0;
    last_tick=0;
    year=1800;
    climate=Moderation;
    players;
    map;
    region;
    cities;
    track;
    stations;
    options;
    random;
    seed
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

let get_station v x y = Station_map.get v.stations x y

let get_region v = v.region

let get_map v = v.map

let get_tile_height v x y = Tilemap.get_tile_height v.map x y

let iter_cities f v = Cities.iter f v.cities

let find_close_city v x y ~range = Cities.find_close v.cities x y ~range

let check_build_track v ~x ~y ~dir ~player =
  match Tilemap.check_build_track v.map ~x ~y ~dir with
  | `Bridge when Trackmap.check_build_stretch v.track ~x ~y ~dir ~player ~length:2 -> `Bridge
  | `Tunnel(length, _) as tun when Trackmap.check_build_stretch v.track ~x ~y ~dir ~player ~length -> tun
  | (`Tunnel(_,g) | `HighGrade g) when Trackmap.check_build_track v.track ~x ~y ~dir ~player -> `HighGrade g
  | (`Ok | `Ferry) as ret when Trackmap.check_build_track v.track ~x ~y ~dir ~player -> ret
  | _ -> `Illegal

let get_money v ~player = Player.get_money v.players.(player)

let _build_tunnel v ~x ~y ~dir ~player ~length =
  let track = Trackmap.build_tunnel v.track ~x ~y ~dir ~player ~length in
  modify_player v ~player (Player.add_track ~length);
  if not @@ CCEqual.physical v.track track then v.track <- track;
  v

let check_build_station v ~x ~y ~player station_type =
  match Trackmap.check_build_station v.track ~x ~y ~player station_type with
  | `Ok -> Tilemap.check_build_station v.map ~x ~y
  | x -> x

let _build_station v ~x ~y station_type ~player =
  (* TODO: graph *)
  let track, build_new_track = Trackmap.build_station v.track ~x ~y station_type in
  let city = find_close_city ~range:100 v x y |> Option.get_exn_or "error" in
  let station = Station.make ~x ~y ~year:v.year ~name:city ~kind:station_type ~player in
  let stations = Station_map.add v.stations x y station in
  if build_new_track then (
    modify_player v ~player (Player.add_track ~length:1)
  );
  if not @@ CCEqual.physical v.track track then v.track <- track;
  if not @@ CCEqual.physical v.stations stations then v.stations <- stations;
  v

let check_build_bridge v ~x ~y ~dir ~player =
  match check_build_track v ~x ~y ~dir ~player with
  | `Bridge -> `Ok
  | _ -> `Illegal

let _build_bridge v ~x ~y ~dir ~player ~kind =
  let track = Trackmap.build_bridge v.track ~x ~y ~dir ~player ~kind in
  modify_player v ~player (Player.add_track ~length:2);
  if not @@ CCEqual.physical v.track track then v.track <- track;
  v

let _build_track v ~x ~y ~dir ~player =
  let track = Trackmap.build_track v.track ~x ~y ~dir ~player in
  modify_player v ~player (Player.add_track ~length:1);
  if not @@ CCEqual.physical v.track track then v.track <- track;
  v

let _build_ferry v ~x ~y ~dir ~player =
  let dx, dy = Dir.to_offsets dir in
  let tile1 = get_tile v x y in
  let tile2 = get_tile v (x+dx) (y+dy) in
  let kind1, kind2 = match tile1, tile2 with
    | Tile.Ocean _ , Ocean _ -> Track.Ferry, Track.Ferry
    | Ocean _, _ -> Ferry, Track
    | _, Ocean _ -> Track, Ferry
    | _ -> assert false
  in
  let track = Trackmap.build_track v.track ~x ~y ~dir ~player ~kind1 ~kind2 in
  modify_player v ~player (Player.add_track ~length:1);
  if not @@ CCEqual.physical v.track track then v.track <- track;
  v

let check_remove_track v ~x ~y ~dir ~player=
  Trackmap.check_remove_track v.track ~x ~y ~dir ~player

let _remove_track v ~x ~y ~dir ~player =
  let track = Trackmap.remove_track v.track ~x ~y ~dir ~player in
  modify_player v ~player (Player.add_track ~length:(-1));
  if not @@ CCEqual.physical v.track track then v.track <- track;
  v

let _improve_station v ~x ~y ~player ~upgrade =
  let stations = 
    match get_station v x y with
    | Some station ->
        let station = Station.add_upgrade station upgrade player in
        Station_map.add v.stations x y station
    | None -> v.stations
  in
  if not @@ CCEqual.physical v.stations stations then v.stations <- stations;
  v
  
let trackmap_iter v f = Trackmap.iter v.track f

  (* Most time-based work happens here *)
let handle_cycle v =
  v.cycle <- v.cycle + 1;

  (* TODO: ai_routines *)
  let difficulty = v.options.difficulty in
  let climate = v.climate in
  let simple_economy =
    not @@ B_options.RealityLevels.mem v.options.reality_levels `ComplexEconomy 
  in
  let msgs =
    if v.cycle mod cycles_station_supply_demand = 0 then (
      Station_map.fold 
        (fun station msgs ->
          Station.check_rate_war_lose_supplies station ~difficulty;
          match Station.update_supply_demand station v.map ~climate ~simple_economy with
          | [] -> msgs
          | _  -> `ChangeDemand(station.x, station.y, msgs)::msgs
        )
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
  v, msgs

let handle_tick v cur_time =
  let delay_mult = B_options.delay_mult_of_speed v.options.speed in
  let tick_delta = delay_mult * tick_ms in
  let new_time = v.last_tick + tick_delta in
  if cur_time >= new_time then (
    v.last_tick <- cur_time;
    handle_cycle v
  )
  else v, []

let _month_of_time time = (time / month_ticks) mod 12

let get_date v = _month_of_time v.time, v.year

module Action = struct
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
    | NoAction -> backend

end


