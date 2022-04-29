open Containers

(* This is the backend. All game-modifying functions go through here *)

(* The actual game (server) state *)
type t = {
  area: Gmap.area;
  map : Gmap.t;
  track: Trackmap.t;
  cities: Gmap.city array;
}
[@@deriving lens]

let default area resources = 
  let map = List.assoc ~eq:(Stdlib.(=)) area resources.Resources.res_maps in
  let cities = List.assoc ~eq:(Stdlib.(=)) area resources.res_cities
    |> List.map (fun (name,x,y) -> {Gmap.name;x;y})
    |> Array.of_list in
  let track = Trackmap.empty Gmap.map_width Gmap.map_height in
  {map; area; cities; track}

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
  Trackmap.check_build_track v.track ~x ~y ~dir ~player

let build_track v ~x ~y ~dir ~player =
  Trackmap.build_track v.track ~x ~y ~dir ~player

let trackmap_iter v f = Trackmap.iter v.track f

module Action = struct
  type t =
    | Build of {x: int; y: int; dir: Dir.t; player: int}

  let run_one backend = function
    | Build {x; y; dir; player} ->
        let track = build_track backend ~x ~y ~dir ~player in
        {backend with track}

  let run_many backend (li:t list) =
    List.fold_left (fun acc x ->
      run_one acc x)
    backend
    li

end


