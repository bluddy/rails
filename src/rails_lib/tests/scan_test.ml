open! Containers
open Dir
module TM = Trackmap
module S = Scan
open Test_common

let player = Constants.player

let print_map (map:TM.t) = TM.yojson_of_t map |> Yojson.Safe.to_string |> print_string

let print scan = S.show scan |> print_string

let track ?(double=false) dirs = 
  let dbl = if double then `Double else `Single in
  Track.make (Dir.Set.of_list dirs) (Track dbl) player

let bridge dirs b_type = 
  Track.make (Dir.Set.of_list dirs) (Bridge b_type) player

let station dirs =
  Track.make (Dir.Set.of_list dirs) (Station `Terminal) player

let%expect_test "print map" =
  let dirs = [Left; Right] in
  let map = TM.empty 3 3
    |> TM.set (0, 1) (track dirs)
    |> TM.set (1, 1) (track dirs)
    |> TM.set (2, 1) (track dirs)
  in
  print_map map;
  [%expect {| {"map":[[4,{"dirs":[["Left"],["Right"]],"kind":["Track",["Single"]],"ixn":false,"player":0}],[5,{"dirs":[["Left"],["Right"]],"kind":["Track",["Single"]],"ixn":false,"player":0}],[3,{"dirs":[["Left"],["Right"]],"kind":["Track",["Single"]],"ixn":false,"player":0}]],"width":3,"height":3} |}]

let%expect_test "scan map ixn" =
  let map = TM.empty 5 5
    |> TM.set (0, 2) (track [Left;Right])
    |> TM.set (1, 2) (track [Left;Right])
    |> TM.set (2, 2) (track [Left;UpRight;DownRight])
  in
  S.scan map (0, 2) player |> print;
  [%expect {|
    (Scan.Track
       [{ Scan.x = 2; y = 2; dist = 2; dir = Dir.Left; search_dir = Dir.Right;
          station = false; double = false }
         ]) |}]

let%expect_test "scan map ixn double" =
  let double = true in
  let map = TM.empty 5 5
    |> TM.set (0, 2) (track [Left;Right] ~double)
    |> TM.set (1, 2) (track [Left;Right] ~double)
    |> TM.set (2, 2) (track [Left;UpRight;DownRight] ~double)
  in
  S.scan map (0, 2) player |> print;
  [%expect {|
    (Scan.Track
       [{ Scan.x = 2; y = 2; dist = 2; dir = Dir.Left; search_dir = Dir.Right;
          station = false; double = true }
         ]) |}]

let%expect_test "scan map ixn partial double" =
  let double = true in
  let map = TM.empty 5 5
    |> TM.set (0, 2) (track [Left;Right] ~double)
    |> TM.set (1, 2) (track [Left;Right])
    |> TM.set (2, 2) (track [Left;UpRight;DownRight] ~double)
  in
  S.scan map (0, 2) player |> print;
  [%expect {|
    (Scan.Track
       [{ Scan.x = 2; y = 2; dist = 2; dir = Dir.Left; search_dir = Dir.Right;
          station = false; double = false }
         ]) |}]

let%expect_test "scan map ixn partial double woodbridge" =
  let double = true in
  let map = TM.empty 5 5
    |> TM.set (0, 2) (track [Left;Right] ~double)
    |> TM.set (1, 2) (bridge [Left;Right] Bridge.Wood)
    |> TM.set (2, 2) (track [Left;UpRight;DownRight] ~double)
  in
  S.scan map (0, 2) player |> print;
  [%expect {|
    (Scan.Track
       [{ Scan.x = 2; y = 2; dist = 2; dir = Dir.Left; search_dir = Dir.Right;
          station = false; double = false }
         ]) |}]

let%expect_test "scan map ixn partial double stonebridge" =
  let double = true in
  let map = TM.empty 5 5
    |> TM.set (0, 2) (track [Left;Right] ~double)
    |> TM.set (1, 2) (bridge [Left;Right] Bridge.Stone)
    |> TM.set (2, 2) (track [Left;UpRight;DownRight] ~double)
  in
  S.scan map (0, 2) player |> print;
  [%expect {|
    (Scan.Track
       [{ Scan.x = 2; y = 2; dist = 2; dir = Dir.Left; search_dir = Dir.Right;
          station = false; double = true }
         ]) |}]

let%expect_test "scan map station" =
  let map = TM.empty 5 5
    |> TM.set (0, 2) (track [Left;Right])
    |> TM.set (1, 2) (track [Left;Right])
    |> TM.set (2, 2) (station [Left;Right])
  in
  S.scan map (0, 2) player |> print;
  [%expect {|
    (Scan.Track
       [{ Scan.x = 2; y = 2; dist = 2; dir = Dir.Left; search_dir = Dir.Right;
          station = true; double = false }
         ]) |}]

let%expect_test "scan map station double" =
  let double = true in
  let map = TM.empty 5 5
    |> TM.set (0, 2) (track [Left;Right] ~double)
    |> TM.set (1, 2) (track [Left;Right] ~double)
    |> TM.set (2, 2) (station [Left;Right])
  in
  S.scan map (0, 2) player |> print;
  [%expect {|
    (Scan.Track
       [{ Scan.x = 2; y = 2; dist = 2; dir = Dir.Left; search_dir = Dir.Right;
          station = true; double = true }
         ]) |}]

let%expect_test "scan map no ixn" =
  let map = TM.empty 5 5
    |> TM.set (0, 2) (track [Left;Right])
    |> TM.set (1, 2) (track [Left;Right])
  in
  S.scan map (0, 2) player |> print;
  [%expect {| (Scan.Track []) |}]


let%expect_test "scan map 2 ixns" =
  let map = TM.empty 4 7
    |> TM.set (0, 3) (track [DownLeft;UpLeft;Right])
    |> TM.set (1, 3) (track [Left;Right])
    |> TM.set (2, 3) (track [Left;UpRight;DownRight])
  in
  S.scan map (1, 3) player |> print;
  [%expect {|
    (Scan.Track
       [{ Scan.x = 0; y = 3; dist = 1; dir = Dir.Right; search_dir = Dir.Left;
          station = false; double = false };
         { Scan.x = 2; y = 3; dist = 1; dir = Dir.Left; search_dir = Dir.Right;
           station = false; double = false }
         ]) |}]

let%expect_test "scan map 2 ixns 1 double" =
  let double = true in
  let map = TM.empty 4 7
    |> TM.set (0, 3) (track [DownLeft;UpLeft;Right] ~double)
    |> TM.set (1, 3) (track [Left;Right] ~double)
    |> TM.set (2, 3) (track [Left;UpRight;DownRight])
  in
  S.scan map (1, 3) player |> print;
  [%expect {|
    (Scan.Track
       [{ Scan.x = 0; y = 3; dist = 1; dir = Dir.Right; search_dir = Dir.Left;
          station = false; double = true };
         { Scan.x = 2; y = 3; dist = 1; dir = Dir.Left; search_dir = Dir.Right;
           station = false; double = false }
         ]) |}]

let%expect_test "scan map 3 stations in a row" =
  let map = TM.empty 20 20
    |> TM.set (1, 5) (station [Left;Right])
    |> TM.set (2, 5) (track [Left;Right])
    |> TM.set (3, 5) (station [Left;Right])
    |> TM.set (4, 5) (track [Left;Right])
    |> TM.set (5, 5) (station [Left;Right])
  in
  S.scan map (3, 5) player |> print;
  [%expect {|
    (Scan.Station
       [{ Scan.x = 1; y = 5; dist = 2; dir = Dir.Right; search_dir = Dir.Left;
          station = true; double = false };
         { Scan.x = 5; y = 5; dist = 2; dir = Dir.Left; search_dir = Dir.Right;
           station = true; double = false }
         ]) |}]

let print_sscan s = Pair.pp Int.pp Track.pp_double Format.std_formatter s

let%expect_test "train_scan map one train" =
  let tracks = TM.empty 20 20
    |> build_road ~y:10 5 15 
  in
  let trains = Trainmap.empty () in
  let trains = Trainmap.add trains @@ dummy_train (8, 10) Right in
  S.scan_station_block tracks trains ~x:5 ~y:10 player Right |> print_sscan;
  [%expect {|
    1,
    `Single |}]

let%expect_test "train_scan map double" =
  let tracks = TM.empty 20 20
    |> build_road ~track:(Track.Track `Double) ~y:10 5 15 
  in
  let trains = Trainmap.empty () in
  let trains = Trainmap.add trains @@ dummy_train (8, 10) Right in
  S.scan_station_block tracks trains ~x:5 ~y:10 player Right |> print_sscan;
  [%expect {|
    1,
    `Double |}]

let%expect_test "train_scan map two trains" =
  let tracks = TM.empty 20 20
    |> build_road ~y:10 5 15 
  in
  let trains = Trainmap.empty () in
  let trains = Trainmap.add trains @@ dummy_train (8, 10) Right in
  let trains = Trainmap.add trains @@ dummy_train (14, 10) Right in
  S.scan_station_block tracks trains ~x:5 ~y:10 player Right |> print_sscan;
  [%expect {|
    2,
    `Single |}]

let%expect_test "train_scan map two trains same place" =
  let tracks = TM.empty 20 20
    |> build_road ~y:10 5 15 
  in
  let trains = Trainmap.empty () in
  let trains = Trainmap.add trains @@ dummy_train (8, 10) Right in
  let trains = Trainmap.add trains @@ dummy_train (8, 10) Right in
  S.scan_station_block tracks trains ~x:5 ~y:10 player Right |> print_sscan;
  [%expect {|
    2,
    `Single |}]

let%expect_test "train_scan map two trains, ixn in middle" =
  let tracks = TM.empty 20 20
    |> build_road ~y:10 5 15 
    |> TM.set ~x:10 ~y:10 ~t:(track [Left;Right;UpRight])
  in
  let trains = Trainmap.empty () in
  let trains = Trainmap.add trains @@ dummy_train (8, 10) Right in
  let trains = Trainmap.add trains @@ dummy_train (14, 10) Right in
  S.scan_station_block tracks trains ~x:5 ~y:10 player Right |> print_sscan;
  [%expect {|
    2,
    `Single |}]

let%expect_test "train_scan map two trains, station in middle" =
  let tracks = TM.empty 20 20
    |> build_road ~y:10 5 15 
    |> TM.set ~x:10 ~y:10 ~t:(station [Left;Right])
  in
  let trains = Trainmap.empty () in
  let trains = Trainmap.add trains @@ dummy_train (8, 10) Right in 
  let trains = Trainmap.add trains @@ dummy_train (14, 10) Right in
  S.scan_station_block tracks trains ~x:5 ~y:10 player Right |> print_sscan;
  (* Get just 1 till end of block *)
  [%expect {|
    1,
    `Single |}]

let%expect_test "train_scan double map two trains, station in middle" =
  let tracks = TM.empty 20 20
    |> build_road ~track:(Track.Track `Double) ~y:10 5 15 
    |> TM.set ~x:10 ~y:10 ~t:(station [Left;Right])
  in
  let trains = Trainmap.empty () in
  let trains = Trainmap.add trains @@ dummy_train (8, 10) Right in 
  let trains = Trainmap.add trains @@ dummy_train (14, 10) Right in
  S.scan_station_block tracks trains ~x:5 ~y:10 player Right |> print_sscan;
  (* Get just 1 till end of block, but double *)
  [%expect {|
    1,
    `Double |}]

