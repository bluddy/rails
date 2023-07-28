open Tilemap
open Containers

let map ~width ~height =
  let map = Array.make (width * height) Tile.Clear in
  let heightmap = Array.empty in
  {seed=0; map; heightmap; width; height; region=EastUS}

let print_map map = yojson_of_t map |> Yojson.Safe.to_string |> print_string

let%expect_test "print map" =
  let map = map ~width:3 ~height:3 in
  print_map map;
  [%expect {| {"seed":0,"map":[["Clear"],["Clear"],["Clear"],["Clear"],["Clear"],["Clear"],["Clear"],["Clear"],["Clear"]],"heightmap":[],"width":3,"height":3,"region":["EastUS"]} |}]

