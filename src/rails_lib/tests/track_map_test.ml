open Trackmap
open! Containers

let print_map map = yojson_of_t map |> Yojson.Safe.to_string |> print_string

let%expect_test "print map" =
  let map = empty 3 3 in
  let track = 
    (Track.make
      (Dir.Set.of_list [Dir.Left; Dir.Right])
      (Track `Single)
      ~player:0)
  in
  let map = set map 0 1 track in
  let map = set map 1 1 track in
  let map = set map 2 1 track in
  print_map map;
  [%expect {| {"map":[[4,{"dirs":[["Left"],["Right"]],"kind":["Track",["Single"]],"ixn":false,"player":0}],[5,{"dirs":[["Left"],["Right"]],"kind":["Track",["Single"]],"ixn":false,"player":0}],[3,{"dirs":[["Left"],["Right"]],"kind":["Track",["Single"]],"ixn":false,"player":0}]],"width":3,"height":3} |}]
    






