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

module Cities = struct
  type t = {
    map: (int, string) Hashtbl.t;
    width: int;
    height: int;
  }

  let make map width height = {map; width; height}

  let iter f v =
    Hashtbl.iter (fun offset city_s ->
      let x, y = Utils.x_y_of_offset v.width offset in
      f x y city_s)
    v.map

  let find v x y = Hashtbl.find_opt v.map (Utils.calc_offset v.width x y)

  let to_list v = Hashtbl.to_list v.map
    |> List.map (fun (i, city) ->
      let x, y = Utils.x_y_of_offset v.width i in
      x, y, city)

  let find_close v x y ~range =
    let res = Utils.scan ~range ~x ~y ~width:v.width ~height:v.height
      ~f:(fun x y -> match find v x y with Some _ -> true | None -> false)
    in
    match res with
    | Some (x, y) -> find v x y
    | None -> None

end

type t = {
  year: int;
  region: Region.t;
  map : Tilemap.t;
  track: Trackmap.t;
  cities: Cities.t;
  stations: Station.Map.t;
  options : options;
}

let default region resources = 
  let map = List.assoc ~eq:(Stdlib.(=)) region resources.Resources.res_maps in
  let cities =
    let h = Hashtbl.create 100 in
    List.assoc ~eq:(Stdlib.(=)) region resources.res_cities
    |> List.iter (fun (name,x,y) -> Hashtbl.replace h (y*Tilemap.map_width + x) name);
    Cities.make h Tilemap.map_width Tilemap.map_height
  in
  let track = Trackmap.empty Tilemap.map_width Tilemap.map_height in
  let speed = `Moderate in
  let reality_levels = RealityLevels.empty in
  let options = {speed; reality_levels} in
  let stations = Station.Map.create Tilemap.map_width in
  {year=1800; map; region; cities; track; stations; options}

let map_height v = Tilemap.get_height v.map

let map_width v = Tilemap.get_width v.map

let get_tile v x y = Tilemap.get_tile v.map x y

let get_track v x y = Trackmap.get v.track x y

let get_cities v = Cities.to_list v.cities

let get_station v x y = Station.Map.get v.stations x y

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

let _build_tunnel v ~x ~y ~dir ~player ~length =
  Trackmap.build_tunnel v.track ~x ~y ~dir ~player ~length

let check_build_station v ~x ~y ~player station_type =
  match Trackmap.check_build_station v.track ~x ~y ~player station_type with
  | `Ok -> Tilemap.check_build_station v.map ~x ~y
  | x -> x

let _build_station v ~x ~y station_type ~player =
  (* TODO: graph *)
  let track = Trackmap.build_station v.track ~x ~y station_type in
  let city = find_close_city ~range:100 v x y |> Option.get_exn_or "error" in
  let station = Station.make ~x ~y ~year:v.year ~city ~kind:station_type ~player in
  let stations = Station.Map.add v.stations x y station in
  track, stations

let check_build_bridge v ~x ~y ~dir ~player =
  match check_build_track v ~x ~y ~dir ~player with
  | `Bridge -> `Ok
  | _ -> `Illegal

let _build_bridge v ~x ~y ~dir ~player ~kind =
  Trackmap.build_bridge v.track ~x ~y ~dir ~player ~kind

let _build_track v ~x ~y ~dir ~player =
  Trackmap.build_track v.track ~x ~y ~dir ~player

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
  Trackmap.build_track v.track ~x ~y ~dir ~player ~kind1 ~kind2

let check_remove_track v ~x ~y ~dir ~player=
  Trackmap.check_remove_track v.track ~x ~y ~dir ~player

let _remove_track v ~x ~y ~dir ~player =
  Trackmap.remove_track v.track ~x ~y ~dir ~player

let trackmap_iter v f = Trackmap.iter v.track f

module Action = struct
  type t =
    | NoAction
    | BuildTrack of Utils.msg
    | BuildFerry of Utils.msg
    | BuildStation of {x: int; y: int; kind: Station.kind; player: int}
    | BuildBridge of Utils.msg * Bridge.t
    | BuildTunnel of Utils.msg * int (* length *)
    | RemoveTrack of Utils.msg

  let run backend = function
    | BuildTrack {x; y; dir; player} ->
        let track = _build_track backend ~x ~y ~dir ~player in
        {backend with track}
    | BuildFerry {x; y; dir; player} ->
        let track = _build_ferry backend ~x ~y ~dir ~player in
        {backend with track}
    | BuildStation {x; y; kind; player} ->
        let track, stations = _build_station backend ~x ~y kind ~player in
        {backend with track; stations}
    | BuildBridge({x; y; dir; player}, kind) ->
        let track = _build_bridge backend ~x ~y ~dir ~kind ~player in
        {backend with track}
    | BuildTunnel({x; y; dir; player}, length) ->
        let track = _build_tunnel backend ~x ~y ~dir ~player ~length in
        {backend with track}
    | RemoveTrack {x; y; dir; player} ->
        let track = _remove_track backend ~x ~y ~dir ~player in
        {backend with track}
    | NoAction -> backend

end


