open! Containers
open Dir
module TM = Trackmap
module S = Scan

let print_map (map:TM.t) = TM.yojson_of_t map |> Yojson.Safe.to_string |> print_string

let tm = Trainmap.empty ()

let track ?(double=false) dirs = 
  let dbl = if double then `Double else `Single in
  Track.make (Dir.Set.of_list dirs) (Track dbl) ~player:0

let bridge dirs b_type = 
  Track.make (Dir.Set.of_list dirs) (Bridge b_type) ~player:0

let%expect_test "print map" =
  let dirs = [Left; Right] in
  let map = TM.empty 3 3
    |> TM.set ~x:0 ~y:1 ~t:(track dirs)
    |> TM.set ~x:1 ~y:1 ~t:(track dirs)
    |> TM.set ~x:2 ~y:1 ~t:(track dirs)
  in
  print_map map;
  [%expect {| {"map":[[4,{"dirs":[["Left"],["Right"]],"kind":["Track",["Single"]],"ixn":false,"player":0}],[5,{"dirs":[["Left"],["Right"]],"kind":["Track",["Single"]],"ixn":false,"player":0}],[3,{"dirs":[["Left"],["Right"]],"kind":["Track",["Single"]],"ixn":false,"player":0}]],"width":3,"height":3} |}]

let%expect_test "scan map ixn" =
  let map = TM.empty 5 5
    |> TM.set ~x:0 ~y:2 ~t:(track [Left;Right])
    |> TM.set ~x:1 ~y:2 ~t:(track [Left;Right])
    |> TM.set ~x:2 ~y:2 ~t:(track [Left;UpRight;DownRight])
  in
  S.scan map tm ~x:0 ~y:2 ~player:0 |> S.show |> print_string;
  [%expect {|
    (Trackmap.Search.Track
       [{ Trackmap.Search.x = 2; y = 2; dist = 2; dir = Dir.Left;
          search_dir = Dir.Right; station = false; double = false }
         ]) |}]

let%expect_test "scan map ixn double" =
  let double = true in
  let map = TM.empty 5 5
    |> TM.set ~x:0 ~y:2 ~t:(track [Left;Right] ~double)
    |> TM.set ~x:1 ~y:2 ~t:(track [Left;Right] ~double)
    |> TM.set ~x:2 ~y:2 ~t:(track [Left;UpRight;DownRight] ~double)
  in
  S.scan map tm ~x:0 ~y:2 ~player:0 |> S.show |> print_string;
  [%expect {|
    (Trackmap.Search.Track
       [{ Trackmap.Search.x = 2; y = 2; dist = 2; dir = Dir.Left;
          search_dir = Dir.Right; station = false; double = true }
         ]) |}]

let%expect_test "scan map ixn partial double" =
  let double = true in
  let map = TM.empty 5 5
    |> TM.set ~x:0 ~y:2 ~t:(track [Left;Right] ~double)
    |> TM.set ~x:1 ~y:2 ~t:(track [Left;Right])
    |> TM.set ~x:2 ~y:2 ~t:(track [Left;UpRight;DownRight] ~double)
  in
  S.scan map tm ~x:0 ~y:2 ~player:0 |> S.show |> print_string;
  [%expect {|
    (Trackmap.Search.Track
       [{ Trackmap.Search.x = 2; y = 2; dist = 2; dir = Dir.Left;
          search_dir = Dir.Right; station = false; double = false }
         ]) |}]

let%expect_test "scan map ixn partial double woodbridge" =
  let double = true in
  let map = TM.empty 5 5
    |> TM.set ~x:0 ~y:2 ~t:(track [Left;Right] ~double)
    |> TM.set ~x:1 ~y:2 ~t:(bridge [Left;Right] Bridge.Wood)
    |> TM.set ~x:2 ~y:2 ~t:(track [Left;UpRight;DownRight] ~double)
  in
  S.scan map tm ~x:0 ~y:2 ~player:0 |> S.show |> print_string;
  [%expect {|
    (Trackmap.Search.Track
       [{ Trackmap.Search.x = 2; y = 2; dist = 2; dir = Dir.Left;
          search_dir = Dir.Right; station = false; double = false }
         ]) |}]

let%expect_test "scan map ixn partial double stonebridge" =
  let double = true in
  let map = TM.empty 5 5
    |> TM.set ~x:0 ~y:2 ~t:(track [Left;Right] ~double)
    |> TM.set ~x:1 ~y:2 ~t:(bridge [Left;Right] Bridge.Stone)
    |> TM.set ~x:2 ~y:2 ~t:(track [Left;UpRight;DownRight] ~double)
  in
  S.scan map tm ~x:0 ~y:2 ~player:0 |> S.show |> print_string;
  [%expect {|
    (Trackmap.Search.Track
       [{ Trackmap.Search.x = 2; y = 2; dist = 2; dir = Dir.Left;
          search_dir = Dir.Right; station = false; double = true }
         ]) |}]

let station dirs =
  Track.make (Dir.Set.of_list dirs) (Station `Terminal) ~player:0

let%expect_test "scan map station" =
  let map = TM.empty 5 5
    |> TM.set ~x:0 ~y:2 ~t:(track [Left;Right])
    |> TM.set ~x:1 ~y:2 ~t:(track [Left;Right])
    |> TM.set ~x:2 ~y:2 ~t:(station [Left;Right])
  in
  S.scan map tm ~x:0 ~y:2 ~player:0 |> S.show |> print_string;
  [%expect {|
    (Trackmap.Search.Track
       [{ Trackmap.Search.x = 2; y = 2; dist = 2; dir = Dir.Left;
          search_dir = Dir.Right; station = true; double = false }
         ]) |}]

let%expect_test "scan map station double" =
  let double = true in
  let map = TM.empty 5 5
    |> TM.set ~x:0 ~y:2 ~t:(track [Left;Right] ~double)
    |> TM.set ~x:1 ~y:2 ~t:(track [Left;Right] ~double)
    |> TM.set ~x:2 ~y:2 ~t:(station [Left;Right])
  in
  S.scan map tm ~x:0 ~y:2 ~player:0 |> S.show |> print_string;
  [%expect {|
    (Trackmap.Search.Track
       [{ Trackmap.Search.x = 2; y = 2; dist = 2; dir = Dir.Left;
          search_dir = Dir.Right; station = true; double = true }
         ]) |}]

let%expect_test "scan map no ixn" =
  let map = TM.empty 5 5
    |> TM.set ~x:0 ~y:2 ~t:(track [Left;Right])
    |> TM.set ~x:1 ~y:2 ~t:(track [Left;Right])
  in
  S.scan map tm ~x:0 ~y:2 ~player:0 |> S.show |> print_string;
  [%expect {| (Trackmap.Search.Track []) |}]


let%expect_test "scan map 2 ixns" =
  let map = TM.empty 4 7
    |> TM.set ~x:0 ~y:3 ~t:(track [DownLeft;UpLeft;Right])
    |> TM.set ~x:1 ~y:3 ~t:(track [Left;Right])
    |> TM.set ~x:2 ~y:3 ~t:(track [Left;UpRight;DownRight])
  in
  S.scan map tm ~x:1 ~y:3 ~player:0 |> S.show |> print_string;
  [%expect {|
    (Trackmap.Search.Track
       [{ Trackmap.Search.x = 0; y = 3; dist = 1; dir = Dir.Right;
          search_dir = Dir.Left; station = false; double = false };
         { Trackmap.Search.x = 2; y = 3; dist = 1; dir = Dir.Left;
           search_dir = Dir.Right; station = false; double = false }
         ]) |}]

let%expect_test "scan map 2 ixns 1 double" =
  let double = true in
  let map = TM.empty 4 7
    |> TM.set ~x:0 ~y:3 ~t:(track [DownLeft;UpLeft;Right] ~double)
    |> TM.set ~x:1 ~y:3 ~t:(track [Left;Right] ~double)
    |> TM.set ~x:2 ~y:3 ~t:(track [Left;UpRight;DownRight])
  in
  S.scan map tm ~x:1 ~y:3 ~player:0 |> S.show |> print_string;
  [%expect {|
    (Trackmap.Search.Track
       [{ Trackmap.Search.x = 0; y = 3; dist = 1; dir = Dir.Right;
          search_dir = Dir.Left; station = false; double = true };
         { Trackmap.Search.x = 2; y = 3; dist = 1; dir = Dir.Left;
           search_dir = Dir.Right; station = false; double = false }
         ]) |}]

