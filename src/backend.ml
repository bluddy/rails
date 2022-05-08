open Containers

(* This is the backend. All game-modifying functions go through here *)

(* The actual game (server) state *)

type speed =
  [`Frozen | `Slow | `Moderate | `Fast | `Turbo]
  [@@deriving enum, eq, show]

type reality_level =
  [`Dispatcher_ops | `Complex_economy | `Cutthroat_competition]
  [@@deriving enum, eq, show]

module RealityLevels = Bitset.Make( struct
  type t = reality_level
  let to_enum = reality_level_to_enum
  let of_enum = reality_level_of_enum
  let last = `Cutthroat_competition
end)

type options = {
  speed: speed;
  reality_levels: RealityLevels.t;
}

type t = {
  area: Gmap.area;
  map : Gmap.t;
  track: Trackmap.t;
  cities: Gmap.city array;
  options : options;
}
[@@deriving lens]

let default area resources = 
  let map = List.assoc ~eq:(Stdlib.(=)) area resources.Resources.res_maps in
  let cities = List.assoc ~eq:(Stdlib.(=)) area resources.res_cities
    |> List.map (fun (name,x,y) -> {Gmap.name;x;y})
    |> Array.of_list in
  let track = Trackmap.empty Gmap.map_width Gmap.map_height in
  let speed = `Moderate in
  let reality_levels = RealityLevels.empty in
  let options = {speed; reality_levels} in
  {map; area; cities; track; options}

let map_height = Gmap.map_height

let map_width = Gmap.map_width

let get_tile v x y = Gmap.get_tile v.map x y

let get_track v x y = Trackmap.get v.track x y

let get_area v = v.area

let get_cities v = v.cities

let get_map v = v.map

let iter_cities f v = 
  Array.iter (fun city -> f city.Gmap.name city.x city.y) v.cities

let check_build_track v ~x ~y ~dir ~player =
  match Gmap.check_build_track v.map ~x ~y ~dir with
  | `Bridge ->
      begin match Trackmap.check_build_bridge v.track ~x ~y ~dir ~player with
      | `Ok -> `Bridge
      | _ -> `Illegal
      end
  | `Ok ->
      Trackmap.check_build_track v.track ~x ~y ~dir ~player
  (* TODO: Ferry, tunnel *)
  | x -> x

let build_track v ~x ~y ~dir ~player =
  Trackmap.build_track v.track ~x ~y ~dir ~player

let check_build_station v ~x ~y ~player station_type =
  match Trackmap.check_build_station v.track ~x ~y ~player station_type with
  | `Ok -> Gmap.check_build_station v.map ~x ~y
  | x -> x

let build_station v ~x ~y station_type =
  (* TODO: create actual station data structure *)
  Trackmap.build_station v.track ~x ~y station_type

let build_bridge v ~x ~y ~dir ~player ~kind =
  Trackmap.build_bridge v.track ~x ~y ~dir ~player ~kind

let trackmap_iter v f = Trackmap.iter v.track f

module Action = struct
  type t =
    | BuildTrack of {x: int; y: int; dir: Dir.t; player: int}
    | BuildStation of {x: int; y: int; kind: Station.t}
    | BuildBridge of {x: int; y: int; dir: Dir.t; kind: [`Wood | `Metal | `Stone]; player: int}

  let run_one backend = function
    | BuildTrack {x; y; dir; player} ->
        let track = build_track backend ~x ~y ~dir ~player in
        {backend with track}
    | BuildStation {x; y; kind} ->
        let track = build_station backend ~x ~y kind in
        {backend with track}
    | BuildBridge {x; y; kind; dir; player} ->
        let track = build_bridge backend ~x ~y ~dir ~kind ~player in
        {backend with track}

  let run_many backend (li:t list) =
    List.fold_left (fun acc x ->
      run_one acc x)
    backend
    li

end

