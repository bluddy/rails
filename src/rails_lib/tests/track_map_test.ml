open! Containers
open Dir
open Trackmap
module S = Trackmap.Search

let print_map (map:Trackmap.t) = yojson_of_t map |> Yojson.Safe.to_string |> print_string

let track dirs = 
  Track.make (Dir.Set.of_list dirs) (Track `Single) ~player:0

let%expect_test "print map" =
  let map = empty 3 3 in
  let dirs = [Left; Right] in
  let map = set map 0 1 @@ track dirs in
  let map = set map 1 1 @@ track dirs in
  let map = set map 2 1 @@ track dirs in
  print_map map;
  [%expect {| {"map":[[4,{"dirs":[["Left"],["Right"]],"kind":["Track",["Single"]],"ixn":false,"player":0}],[5,{"dirs":[["Left"],["Right"]],"kind":["Track",["Single"]],"ixn":false,"player":0}],[3,{"dirs":[["Left"],["Right"]],"kind":["Track",["Single"]],"ixn":false,"player":0}]],"width":3,"height":3} |}]

let%expect_test "scan map ixn" =
  let map = empty 5 5 in
  let map = set map 0 2 @@ track [Left;Right] in
  let map = set map 1 2 @@ track [Left;Right] in
  let map = set map 2 2 @@ track [Left;UpRight;DownRight] in
  S.scan map ~x:0 ~y:2 ~player:0 |> S.show_scan |> print_string;
  [%expect {|
    (Trackmap.Search.Track
       [{ Trackmap.Search.x = 2; y = 2; dist = 2; dir = Dir.Left;
          search_dir = Dir.Right; station = false }
         ]) |}]

let station dirs =
  Track.make (Dir.Set.of_list dirs) (Station `Terminal) ~player:0

let%expect_test "scan map station" =
  let map = empty 5 5 in
  let map = set map 0 2 @@ track [Left;Right] in
  let map = set map 1 2 @@ track [Left;Right] in
  let map = set map 2 2 @@ station [Left;Right] in
  S.scan map ~x:0 ~y:2 ~player:0 |> S.show_scan |> print_string;
  [%expect {|
    (Trackmap.Search.Track
       [{ Trackmap.Search.x = 2; y = 2; dist = 2; dir = Dir.Left;
          search_dir = Dir.Right; station = true }
         ]) |}]

let%expect_test "scan map no ixn" =
  let map = empty 5 5 in
  let map = set map 0 2 @@ track [Left;Right] in
  let map = set map 1 2 @@ track [Left;Right] in
  S.scan map ~x:0 ~y:2 ~player:0 |> S.show_scan |> print_string;
  [%expect {| (Trackmap.Search.Track []) |}]


let%expect_test "scan map 2 ixns" =
  let map = empty 4 7 in
  let map = set map 0 3 @@ track [DownLeft;UpLeft;Right] in
  let map = set map 1 3 @@ track [Left;Right] in
  let map = set map 2 3 @@ track [Left;UpRight;DownRight] in
  S.scan map ~x:1 ~y:3 ~player:0 |> S.show_scan |> print_string;
  [%expect {|
    (Trackmap.Search.Track
       [{ Trackmap.Search.x = 0; y = 3; dist = 1; dir = Dir.Right;
          search_dir = Dir.Left; station = false };
         { Trackmap.Search.x = 2; y = 3; dist = 1; dir = Dir.Left;
           search_dir = Dir.Right; station = false }
         ]) |}]

