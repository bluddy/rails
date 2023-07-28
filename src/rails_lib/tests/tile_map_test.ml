open Tilemap
open Containers

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
  set_tile map 1 0 (Tile.River dirs);
  set_tile map 1 1 (Tile.River dirs);
  set_tile map 1 2 (Tile.River dirs);
  map

let%expect_test "print river map" =
  print_map @@ river_map ();
  [%expect{| {"seed":0,"map":[["Clear"],["River",[["Down"],["Up"]]],["Clear"],["Clear"],["River",[["Down"],["Up"]]],["Clear"],["Clear"],["River",[["Down"],["Up"]]],["Clear"]],"heightmap":[43,49,43,49,57,51,43,51,47],"width":3,"height":3,"region":["EastUS"]} |}]

let%test "check track across river" =
  let map = river_map () in
  match check_build_track map ~x:0 ~y:1 ~dir:Dir.Right ~difficulty:`Financier with
  | `Bridge -> true
  | _ -> false

let river_bend () =
  let map = map (3,3) in
  set_tile map 1 0 (Tile.River (Dir.Set.of_list [Dir.Up; Dir.Down]));
  set_tile map 1 1 (Tile.River (Dir.Set.of_list [Dir.Up; Dir.Left]));
  set_tile map 0 1 (Tile.River (Dir.Set.of_list [Dir.Left; Dir.Right]));
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
  check_build_track map ~x:0 ~y:0 ~dir:Dir.DownRight ~difficulty:`Financier |> s_of_response |> print_string;
  [%expect {| Illegal |}]

let%expect_test "river track down" =
  let map = river_bend () in
  check_build_track map ~x:0 ~y:0 ~dir:Dir.Down ~difficulty:`Financier |> s_of_response |> print_string;
  [%expect {| Bridge |}]

let hills_east () =
  let map = map (3,3) in
  set_tile map 2 0 Tile.Hills;
  set_tile map 2 1 Tile.Hills;
  set_tile map 2 2 Tile.Hills;
  map |> update_heightmap

let%expect_test "hills on east" =
  let map = hills_east () in
  check_build_track map ~x:0 ~y:1 ~dir:Dir.Right ~difficulty:`Financier |> s_of_response |> print_string;
  [%expect {| HighGrade 21 |}]

let mountains_east () =
  let map = map (3,3) in
  set_tile map 2 0 Tile.Mountains;
  set_tile map 2 1 Tile.Mountains;
  set_tile map 2 2 Tile.Mountains;
  map |> update_heightmap

let%expect_test "mountains on east" =
  let map = mountains_east () in
  check_build_track map ~x:0 ~y:1 ~dir:Dir.Right ~difficulty:`Financier |> s_of_response |> print_string;
  [%expect {| Tunnel 42 |}]
  
let%expect_test "ocean on east" =
  let map = map (2,1) in
  set_tile map 1 0 @@ Tile.Ocean dir_up_down;
  check_build_track map ~x:0 ~y:0 ~dir:Dir.Right ~difficulty:`Financier |> s_of_response |> print_string;
  [%expect {| Ferry |}]

