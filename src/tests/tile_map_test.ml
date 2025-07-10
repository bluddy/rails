open Containers
module R = Rails_lib
open R.Tilemap
module Tile = R.Tile
module Dir = R.Dir
module Params = R.Params

let map (width,height) = 
  let map = Array.make (width * height) Tile.Clear in
  let heightmap = Array.empty in
  let map = {seed=0; map; heightmap; width; height; region=EastUS} in
  update_heightmap map

let dir_up_down = Dir.Set.of_list [Dir.Up; Dir.Down]

let print_map map = yojson_of_t map |> Yojson.Safe.to_string |> print_string

let%expect_test "print map" =
  let map = map (3,3) in
  print_map map;
  [%expect {| {"seed":0,"map":[["Clear"],["Clear"],["Clear"],["Clear"],["Clear"],["Clear"],["Clear"],["Clear"],["Clear"]],"heightmap":[43,49,43,49,57,51,43,51,47],"width":3,"height":3,"region":["EastUS"]} |}]

(** River tests **)

let river_map () =
  let map = map (3,3) |> update_heightmap in
  let dirs = Dir.Set.of_list [Dir.Up; Dir.Down] in
  set_tile (1, 0) (Tile.River dirs) map;
  set_tile (1, 1) (Tile.River dirs) map;
  set_tile (1, 2) (Tile.River dirs) map;
  map

let%expect_test "print river map" =
  print_map @@ river_map ();
  [%expect{| {"seed":0,"map":[["Clear"],["River",[["Down"],["Up"]]],["Clear"],["Clear"],["River",[["Down"],["Up"]]],["Clear"],["Clear"],["River",[["Down"],["Up"]]],["Clear"]],"heightmap":[43,49,43,49,57,51,43,51,47],"width":3,"height":3,"region":["EastUS"]} |}]

let params = Params.make ()

let%test "check track across river" =
  let map = river_map () in
  match check_build_track (0, 1) ~dir:Dir.Right params map with
  | `Bridge -> true
  | _ -> false

let river_bend () =
  let map = map (3,3) in
  set_tile (1, 0) (Tile.River (Dir.Set.of_list [Dir.Up; Dir.Down])) map;
  set_tile (1, 1) (Tile.River (Dir.Set.of_list [Dir.Up; Dir.Left])) map;
  set_tile (0, 1) (Tile.River (Dir.Set.of_list [Dir.Left; Dir.Right])) map;
  map |> update_heightmap

let s_of_response = function
  | `Bridge -> "Bridge"
  | `Ferry -> "Ferry"
  | `HighGrade i -> Printf.sprintf "HighGrade %d" i
  | `Illegal -> "Illegal"
  | `Ok -> "Ok"
  | `Tunnel i -> Printf.sprintf "Tunnel %d" i

let%expect_test "river track diag bend" =
  let map = river_bend () in
  check_build_track (0, 0) ~dir:Dir.DownRight params map |> s_of_response |> print_string;
  [%expect {| Illegal |}]

let%expect_test "river track down" =
  let map = river_bend () in
  check_build_track (0, 0) ~dir:Dir.Down params map |> s_of_response |> print_string;
  [%expect {| Bridge |}]

let hills_east () =
  let map = map (3,3) in
  set_tile (2, 0) Tile.Hills map;
  set_tile (2, 1) Tile.Hills map;
  set_tile (2, 2) Tile.Hills map;
  map |> update_heightmap

let%expect_test "hills on east" =
  let map = hills_east () in
  check_build_track (0, 1) ~dir:Dir.Right params map |> s_of_response |> print_string;
  [%expect {| HighGrade 43 |}]

let mountains_east () =
  let map = map (3,3) in
  set_tile (2, 0) Tile.Mountains map;
  set_tile (2, 1) Tile.Mountains map;
  set_tile (2, 2) Tile.Mountains map;
  map |> update_heightmap

let%expect_test "mountains on east" =
  let map = mountains_east () in
  check_build_track (0, 1) ~dir:Dir.Right params map |> s_of_response |> print_string;
  [%expect {| Tunnel 85 |}]
  
let%expect_test "ocean on east" =
  let map = map (2,1) in
  set_tile (1, 0) (Tile.Ocean dir_up_down) map;
  check_build_track (0, 0) ~dir:Dir.Right params map |> s_of_response |> print_string;
  [%expect {| Ferry |}]

