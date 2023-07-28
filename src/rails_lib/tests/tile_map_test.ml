open Tilemap
open Containers

let map (width,height) = 
  let map = Array.make (width * height) Tile.Clear in
  let heightmap = Array.empty in
  {seed=0; map; heightmap; width; height; region=EastUS}

let print_map map = yojson_of_t map |> Yojson.Safe.to_string |> print_string

let%expect_test "print map" =
  let map = map (3,3) in
  print_map map;
  [%expect {| {"seed":0,"map":[["Clear"],["Clear"],["Clear"],["Clear"],["Clear"],["Clear"],["Clear"],["Clear"],["Clear"]],"heightmap":[],"width":3,"height":3,"region":["EastUS"]} |}]

let river_map () =
  let map = map (3,3) in
  let dirs = Dir.Set.of_list [Dir.Up; Dir.Down] in
  set_tile map 1 0 (Tile.River dirs);
  set_tile map 1 1 (Tile.River dirs);
  set_tile map 1 2 (Tile.River dirs);
  map

let%expect_test "print river map" =
  print_map @@ river_map ();
  [%expect{| {"seed":0,"map":[["Clear"],["River",[["Down"],["Up"]]],["Clear"],["Clear"],["River",[["Down"],["Up"]]],["Clear"],["Clear"],["River",[["Down"],["Up"]]],["Clear"]],"heightmap":[],"width":3,"height":3,"region":["EastUS"]} |}]

  

