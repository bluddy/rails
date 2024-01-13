open Containers
module TG = Track_graph

let graph () =
  TG.make ()
  |> TG.add_ixn ~x:1 ~y:2
  |> TG.add_ixn ~x:3 ~y:4
  |> TG.add_segment ~xyd1:(1,2,Dir.Up) ~xyd2:(3,4,Down) ~dist:5 ~double:false

let%expect_test "iter_succ_ixn_dirs" =
  let graph = graph () in
  TG.iter_succ_ixn_dirs (fun _ixn dir ->
    print_string @@ Dir.show dir
  ) graph ~ixn:(1,2);
  [%expect {| Dir.Down |}]

let graph ?(double=false) () =
  TG.make ()
  |> TG.add_ixn ~x:1 ~y:2
  |> TG.add_ixn ~x:3 ~y:4
  |> TG.add_ixn ~x:5 ~y:6
  |> TG.add_ixn ~x:7 ~y:8
  |> TG.add_segment ~xyd1:(1,2,Dir.Up) ~xyd2:(3,4,Right) ~dist:5 ~double
  |> TG.add_segment ~xyd1:(1,2,Dir.UpRight) ~xyd2:(5,6,Left) ~dist:10 ~double
  |> TG.add_segment ~xyd1:(5,6,Dir.UpRight) ~xyd2:(7,8,DownLeft) ~dist:3 ~double

let print_graph g =
  TG.yojson_of_t g |> Yojson.Safe.to_string |> print_string

let%expect_test "graph print" =
  let g = graph () in
  print_graph g;
  [%expect {| [[[3,4],[1,2],{"nodes":[[[1,2],["Up"]],[[3,4],["Right"]]],"dist":5,"double":false,"block":false}],[[5,6],[1,2],{"nodes":[[[1,2],["UpRight"]],[[5,6],["Left"]]],"dist":10,"double":false,"block":false}],[[7,8],[5,6],{"nodes":[[[5,6],["UpRight"]],[[7,8],["DownLeft"]]],"dist":3,"double":false,"block":false}]] |}]

let%expect_test "double graph print" =
  let g = graph ~double:true () in
  print_graph g;
  [%expect {| [[[3,4],[1,2],{"nodes":[[[1,2],["Up"]],[[3,4],["Right"]]],"dist":5,"double":true,"block":false}],[[5,6],[1,2],{"nodes":[[[1,2],["UpRight"]],[[5,6],["Left"]]],"dist":10,"double":true,"block":false}],[[7,8],[5,6],{"nodes":[[[5,6],["UpRight"]],[[7,8],["DownLeft"]]],"dist":3,"double":true,"block":false}]] |}]

let%expect_test "graph remove segment" =
  let g = graph ()
    |> TG.remove_segment ~xyd:(1,2,Dir.Up)
  in
  print_graph g;
  [%expect {| [[[5,6],[1,2],{"nodes":[[[1,2],["UpRight"]],[[5,6],["Left"]]],"dist":10,"double":false,"block":false}],[[7,8],[5,6],{"nodes":[[[5,6],["UpRight"]],[[7,8],["DownLeft"]]],"dist":3,"double":false,"block":false}]] |}]

let%expect_test "graph find ixn from ixn" =
  let g = graph () in
  let res = TG.find_ixn_from_ixn_dir g ~ixn:(3,4) ~dir:Down in
  Option.pp Utils.pp_loc Format.std_formatter res;
  [%expect {| None |}]

let%expect_test "graph shortest path" =
  let g = graph ()
    |> TG.add_segment ~xyd1:(3,4,UpRight) ~xyd2:(5,6,DownRight) ~dist:8 ~double:false
  in
  let res = TG.shortest_path g ~src:(1,2) ~dest:(5,6) in
  Option.pp Dir.pp Format.std_formatter res;
  [%expect {| Some Dir.UpRight |}]

let%expect_test "graph shortest path" =
  let g = graph ()
    |> TG.add_segment ~xyd1:(3,4,UpRight) ~xyd2:(5,6,DownRight) ~dist:2 ~double:false
  in
  let res = TG.shortest_path g ~src:(1,2) ~dest:(5,6) in
  Option.pp Dir.pp Format.std_formatter res;
  [%expect {| Some Dir.Up |}]

let%expect_test "connected_stations_dirs" =
  let t = Track.make Dir.Set.empty (Station `Station) ~player:0 in
  let map = Trackmap.empty 10 10
    |> Trackmap.set ~x:1 ~y:2 ~t
    |> Trackmap.set ~x:3 ~y:4 ~t
    |> Trackmap.set ~x:5 ~y:6 ~t
  in
  let g = graph () in
  let res = TG.connected_stations_dirs g map (1,2) |> Iter.to_list in
  List.pp (Pair.pp (Pair.pp Int.pp Int.pp) Dir.pp) Format.std_formatter res;
  [%expect {|
    3, 4, Dir.Right, 5, 6,
    Dir.Left |}]

module Track = struct
  module TM = Trackmap

  let track ?(double=false) dirs = 
    let double = if double then `Double else `Single in
    Track.make (Dir.Set.of_list dirs) (Track double) ~player:0

  let station dirs = 
    Track.make (Dir.Set.of_list dirs) (Station `Station) ~player:0

  let dirs = [Dir.Left; Right]
  let y = 2
  (* x---- *)
  let std_map ?(double=false) () =
    TM.empty 7 7
    |> TM.set ~x:1 ~y ~t:(track ~double @@ UpLeft::dirs)
    |> TM.set ~x:2 ~y ~t:(track ~double dirs)
    |> TM.set ~x:3 ~y ~t:(track ~double dirs)
    |> TM.set ~x:4 ~y ~t:(track ~double dirs)
    |> TM.set ~x:5 ~y ~t:(track ~double [Left])

  let%expect_test "build_station at end of track" =
    (* x---- map *)
    let map = std_map () in
    let scan1 = TM.Search.scan map ~x:5 ~y ~player:0 in
    (* Add station x x---s *)
    let map = TM.set map ~x:5 ~y ~t:(station dirs) in
    let scan2 = TM.Search.scan map ~x:5 ~y ~player:0 in
    (* Corresponding graph *)
    let g = TG.make ()
      |> TG.add_ixn ~x:1 ~y
    in
    print_graph g;
    [%expect {| [] |}];
    let g = TG.Track.handle_build_station g ~x:5 ~y scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"double":false,"block":false}]] |}]

  let%expect_test "build_station mid track" =
    (* x---x -> x-s-x *)
    let map = std_map ()
      |> TM.set ~x:5 ~y ~t:(track @@ UpRight::dirs)
    in
    let scan1 = TM.Search.scan map ~x:3 ~y ~player:0 in
    let map = TM.set map ~x:3 ~y ~t:(station dirs) in
    let scan2 = TM.Search.scan map ~x:3 ~y ~player:0 in
    (* Corresponding graph *)
    let g = TG.make ()
      |> TG.add_ixn ~x:1 ~y
      |> TG.add_ixn ~x:4 ~y
      |> TG.add_segment ~xyd1:(1,1,Right) ~xyd2:(5,1,Left) ~dist:5 ~double:false
    in
    print_graph g;
    [%expect {| [[[5,1],[1,1],{"nodes":[[[1,1],["Right"]],[[5,1],["Left"]]],"dist":5,"double":false,"block":false}]] |}];
    let g = TG.Track.handle_build_station g ~x:3 ~y scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[3,2],{"nodes":[[[3,2],["Right"]],[[5,2],["Left"]]],"dist":2,"double":false,"block":false}],[[5,1],[1,1],{"nodes":[[[1,1],["Right"]],[[5,1],["Left"]]],"dist":5,"double":false,"block":false}],[[3,2],[1,2],{"nodes":[[[1,2],["Right"]],[[3,2],["Left"]]],"dist":2,"double":false,"block":false}]] |}]

  let%expect_test "build_track_simple" =
    (* x--- x -> x---x *)
    let map = std_map ()
      |> TM.set ~x:4 ~y ~t:(track [Left])
      |> TM.set ~x:5 ~y ~t:(track [Left; Right; UpRight])
    in
    let scan1 = TM.Search.scan map ~x:4 ~y ~player:0 in
    let map = TM.set map ~x:4 ~y ~t:(track [Left;Right]) in
    let scan2 = TM.Search.scan map ~x:4 ~y ~player:0 in
    let g = TG.make () in
    print_graph g;
    [%expect {| [] |}];
    let g = TG.Track.handle_build_track_simple g scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"double":false,"block":false}]] |}]

    (* same as above, just handle_build_track *)
  let%expect_test "build_track connect ixn" =
    (* x--- x -> x---x *)
    let map = std_map ()
      |> TM.set ~x:4 ~y ~t:(track [Left])
      |> TM.set ~x:5 ~y ~t:(track [Left; Right; UpRight])
    in
    let scan1 = TM.Search.scan map ~x:4 ~y ~player:0 in
    let map = TM.set map ~x:4 ~y ~t:(track [Left;Right]) in
    let scan2 = TM.Search.scan map ~x:4 ~y ~player:0 in
    let g = TG.make () in
    print_graph g;
    [%expect {| [] |}];
    let g = TG.Track.handle_build_track g ~x:4 ~y scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"double":false,"block":false}]] |}]

  let%expect_test "build_track create ixn at end" =
    (* x----  -> x---x *)
    let map = std_map () in
    let scan1 = TM.Search.scan map ~x:5 ~y ~player:0 in
    let map = TM.set map ~x:5 ~y ~t:(track [Left; Right; UpRight]) in
    let scan2 = TM.Search.scan map ~x:5 ~y ~player:0 in
    let g = TG.make () in
    print_graph g;
    [%expect {| [] |}];
    let g = TG.Track.handle_build_track g ~x:5 ~y scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"double":false,"block":false}]] |}]

  let%expect_test "build_track create ixn in middle" =
     (* x---x  -> x-x-x *)
    let map = std_map ()
      |> TM.set ~x:5 ~y ~t:(track [Left; Right; UpRight])
    in
    let scan1 = TM.Search.scan map ~x:3 ~y ~player:0 in
    let map = TM.set map ~x:3 ~y ~t:(track [Left; Right; UpRight]) in
    let scan2 = TM.Search.scan map ~x:3 ~y ~player:0 in
    let g = TG.make ()
      |> TG.add_segment ~xyd1:(1,y,Right) ~xyd2:(5,y,Left) ~dist:4 ~double:false
    in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"double":false,"block":false}]] |}];
    let g = TG.Track.handle_build_track g ~x:3 ~y scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[3,2],{"nodes":[[[3,2],["Right"]],[[5,2],["Left"]]],"dist":2,"double":false,"block":false}],[[3,2],[1,2],{"nodes":[[[1,2],["Right"]],[[3,2],["Left"]]],"dist":2,"double":false,"block":false}]] |}]

  let%expect_test "build_track create double track in middle" =
     (* x---x  -> x-=-x *)
    let double = true in
    let map = std_map () |> TM.set ~x:5 ~y ~t:(track ~double [Left; Right; UpRight]) in
    let scan1 = TM.Search.scan map ~x:3 ~y ~player:0 in
    let g = TG.make () |> TG.add_segment ~xyd1:(1,y,Right) ~xyd2:(5,y,Left) ~dist:4 ~double:false in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"double":false,"block":false}]] |}];
    let map = TM.set map ~x:1 ~y ~t:(track ~double [Left; Right; UpRight]) in
    let map = TM.set map ~x:2 ~y ~t:(track ~double [Left; Right]) in
    let map = TM.set map ~x:3 ~y ~t:(track ~double [Left; Right]) in
    let map = TM.set map ~x:4 ~y ~t:(track ~double [Left; Right]) in
    let scan2 = TM.Search.scan map ~x:3 ~y ~player:0 in
    let g = TG.Track.handle_change_double_track g scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"double":true,"block":false}]] |}]

  let%expect_test "build_track create ixn in middle (with stations)" =
     (* s---s  -> s-x-s *)
    let map = std_map ()
      |> TM.set ~x:1 ~y ~t:(station [Left; Right])
      |> TM.set ~x:5 ~y ~t:(station [Left; Right])
    in
    let scan1 = TM.Search.scan map ~x:3 ~y ~player:0 in
    let map = TM.set map ~x:3 ~y ~t:(track [Left; Right; UpRight]) in
    let scan2 = TM.Search.scan map ~x:3 ~y ~player:0 in
    let g = TG.make ()
      |> TG.add_segment ~xyd1:(1,y,Right) ~xyd2:(5,y,Left) ~dist:4 ~double:false
    in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"double":false,"block":false}]] |}];
    let g = TG.Track.handle_build_track g ~x:3 ~y scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[3,2],{"nodes":[[[3,2],["Right"]],[[5,2],["Left"]]],"dist":2,"double":false,"block":false}],[[3,2],[1,2],{"nodes":[[[1,2],["Right"]],[[3,2],["Left"]]],"dist":2,"double":false,"block":false}]] |}]

  let%expect_test "build_track create+connect to another ixn" =
     (*    x          x
          /          /
       x---x  -> x-x-x *)
    let map = std_map ()
      |> TM.set ~x:5 ~y ~t:(track [Left; Right; UpRight])
      |> TM.set ~x:4 ~y:(y-1) ~t:(track [DownLeft; UpRight])
      |> TM.set ~x:5 ~y:(y-2) ~t:(station [DownLeft; UpRight])
    in
    let scan1 = TM.Search.scan map ~x:3 ~y ~player:0 in
    let map = TM.set map ~x:3 ~y ~t:(track [Left; Right; UpRight]) in
    let scan2 = TM.Search.scan map ~x:3 ~y ~player:0 in
    let g = TG.make ()
      |> TG.add_segment ~xyd1:(1,y,Right) ~xyd2:(5,y,Left) ~dist:4 ~double:false
    in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"double":false,"block":false}]] |}];
    let g = TG.Track.handle_build_track g ~x:3 ~y scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[3,2],{"nodes":[[[3,2],["Right"]],[[5,2],["Left"]]],"dist":2,"double":false,"block":false}],[[5,0],[3,2],{"nodes":[[[3,2],["UpRight"]],[[5,0],["DownLeft"]]],"dist":2,"double":false,"block":false}],[[3,2],[1,2],{"nodes":[[[1,2],["Right"]],[[3,2],["Left"]]],"dist":2,"double":false,"block":false}]] |}]

  let%expect_test "build_track create+connect to another double ixn" =
     (*    s          s
          /          /
       x---x  -> x-x-x *)
    let double = true in
    let map = std_map ()
      |> TM.set ~x:5 ~y ~t:(track [Left; Right; UpRight])
      |> TM.set ~x:4 ~y:(y-1) ~t:(track ~double [DownLeft; UpRight])
      |> TM.set ~x:5 ~y:(y-2) ~t:(station [DownLeft; UpRight])
    in
    let scan1 = TM.Search.scan map ~x:3 ~y ~player:0 in
    let map = TM.set map ~x:3 ~y ~t:(track ~double [Left; Right; UpRight]) in
    let scan2 = TM.Search.scan map ~x:3 ~y ~player:0 in
    let g = TG.make ()
      |> TG.add_segment ~xyd1:(1,y,Right) ~xyd2:(5,y,Left) ~dist:4 ~double:false
    in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"double":false,"block":false}]] |}];
    let g = TG.Track.handle_build_track g ~x:3 ~y scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[3,2],{"nodes":[[[3,2],["Right"]],[[5,2],["Left"]]],"dist":2,"double":false,"block":false}],[[5,0],[3,2],{"nodes":[[[3,2],["UpRight"]],[[5,0],["DownLeft"]]],"dist":2,"double":true,"block":false}],[[3,2],[1,2],{"nodes":[[[1,2],["Right"]],[[3,2],["Left"]]],"dist":2,"double":false,"block":false}]] |}]

  let%expect_test "remove_track full" = 
     (* x---x  -> x- -x *)
    let map = std_map ()
      |> TM.set ~x:5 ~y ~t:(track [Left; Right; UpRight])
    in
    let scan1 = TM.Search.scan map ~x:3 ~y ~player:0 in
    let map = TM.remove map ~x:3 ~y in
    let scan2 = TM.Search.scan map ~x:3 ~y ~player:0 in
    let g = TG.make ()
      |> TG.add_segment ~xyd1:(1,y,Right) ~xyd2:(5,y,Left) ~dist:4 ~double:false
    in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"double":false,"block":false}]] |}];
    let g = TG.Track.handle_remove_track g ~x:3 ~y scan1 scan2 in
    print_graph g;
    [%expect {| [] |}]

  let%expect_test "remove_track partial" = 
     (* x---x  -> x- -x *)
    let map = std_map ()
      |> TM.set ~x:5 ~y ~t:(track [Left; Right; UpRight])
    in
    let scan1 = TM.Search.scan map ~x:3 ~y ~player:0 in
    let map = TM.set map ~x:3 ~y ~t:(track [Left]) in
    let scan2 = TM.Search.scan map ~x:3 ~y ~player:0 in
    let g = TG.make ()
      |> TG.add_segment ~xyd1:(1,y,Right) ~xyd2:(5,y,Left) ~dist:4 ~double:false
    in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"double":false,"block":false}]] |}];
    let g = TG.Track.handle_remove_track g ~x:3 ~y scan1 scan2 in
    print_graph g;
    [%expect {| [] |}]

  let%expect_test "remove station" = 
     (* x---S  -> x--- *)
    let map = std_map ()
      |> TM.set ~x:5 ~y ~t:(station [Left; Right])
    in
    let scan1 = TM.Search.scan map ~x:5 ~y ~player:0 in
    let map = TM.set map ~x:5 ~y ~t:(track [Left]) in
    let scan2 = TM.Search.scan map ~x:5 ~y ~player:0 in
    let g = TG.make ()
      |> TG.add_segment ~xyd1:(1,y,Right) ~xyd2:(5,y,Left) ~dist:4 ~double:false
    in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"double":false,"block":false}]] |}];
    let g = TG.Track.handle_remove_track g ~x:5 ~y scan1 scan2 in
    print_graph g;
    [%expect {| [] |}]

  let%expect_test "remove ixn" = 
     (* x---x  -> x--- *)
    let map = std_map ()
      |> TM.set ~x:5 ~y ~t:(station [Left; Right; UpRight])
    in
    let x = 5 in
    let scan1 = TM.Search.scan map ~x ~y ~player:0 in
    let map = TM.set map ~x ~y ~t:(track [Left]) in
    let scan2 = TM.Search.scan map ~x ~y ~player:0 in
    let g = TG.make ()
      |> TG.add_segment ~xyd1:(1,y,Right) ~xyd2:(x,y,Left) ~dist:4 ~double:false
    in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"double":false,"block":false}]] |}];
    let g = TG.Track.handle_remove_track g ~x ~y scan1 scan2 in
    print_graph g;
    [%expect {| [] |}]

  let%expect_test "change double track" = 
    let double = true in
     (* x=-=x  -> x===x *)
    let map = std_map ~double ()
      |> TM.set ~x:5 ~y ~t:(track ~double [Left; Right; UpRight])
      |> TM.set ~x:3 ~y ~t:(track [Left; Right])
    in
    let x = 5 in
    let scan1 = TM.Search.scan map ~x:3 ~y ~player:0 in
    let g = TG.make () |> TG.add_segment ~xyd1:(1,y,Right) ~xyd2:(x,y,Left) ~dist:4 ~double:false in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"double":false,"block":false}]] |}];
    let map = TM.set map ~x:3 ~y ~t:(track ~double [Left; Right]) in
    let scan2 = TM.Search.scan map ~x:3 ~y ~player:0 in
    let g = TG.Track.handle_change_double_track g scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"double":true,"block":false}]] |}]

  let%expect_test "remove double track" = 
    let double = true in
     (* x===x  -> x=-=x *)
    let map = std_map ~double () |> TM.set ~x:5 ~y ~t:(track ~double [Left; Right; UpRight]) in
    let x = 5 in
    let scan1 = TM.Search.scan map ~x:3 ~y ~player:0 in
    let g = TG.make () |> TG.add_segment ~xyd1:(1,y,Right) ~xyd2:(x,y,Left) ~dist:4 ~double:true in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"double":true,"block":false}]] |}];
    let map = TM.set map ~x:3 ~y ~t:(track [Left; Right]) in
    let scan2 = TM.Search.scan map ~x:3 ~y ~player:0 in
    let g = TG.Track.handle_change_double_track g scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"double":false,"block":false}]] |}]

  let%expect_test "remove ixn in middle" = 
     (* x-x-x  -> x---x*)
    let map = std_map ()
      |> TM.set ~x:5 ~y ~t:(track [Left; Right; UpRight])
      |> TM.set ~x:3 ~y ~t:(track [Left; Right; UpRight])
    in
    let x = 3 in
    let scan1 = TM.Search.scan map ~x ~y ~player:0 in
    let map = TM.set map ~x ~y ~t:(track [Left;Right]) in
    let scan2 = TM.Search.scan map ~x ~y ~player:0 in
    let g = TG.make ()
      |> TG.add_segment ~xyd1:(1,y,Right) ~xyd2:(3,y,Left) ~dist:2 ~double:false
      |> TG.add_segment ~xyd1:(3,y,Right) ~xyd2:(5,y,Left) ~dist:2 ~double:false
    in
    print_graph g;
    [%expect {| [[[5,2],[3,2],{"nodes":[[[3,2],["Right"]],[[5,2],["Left"]]],"dist":2,"double":false,"block":false}],[[3,2],[1,2],{"nodes":[[[1,2],["Right"]],[[3,2],["Left"]]],"dist":2,"double":false,"block":false}]] |}];
    let g = TG.Track.handle_remove_track g ~x ~y scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"double":false,"block":false}]] |}]

  let%expect_test "remove ixn in middle" = 
     (*  x         x
       x-x-x  -> x---x*)
    let map = std_map ()
      |> TM.set ~x:5 ~y ~t:(track [Left; Right; UpRight])
      |> TM.set ~x:3 ~y ~t:(track [Left; Right; UpRight])
      |> TM.set ~x:4 ~y:(y-1) ~t:(track [DownLeft; Up; UpRight])
    in
    let x = 3 in
    let scan1 = TM.Search.scan map ~x ~y ~player:0 in
    let map = TM.set map ~x ~y ~t:(track [Left;Right]) in
    let scan2 = TM.Search.scan map ~x ~y ~player:0 in
    let g = TG.make ()
      |> TG.add_segment ~xyd1:(1,y,Right) ~xyd2:(3,y,Left) ~dist:2 ~double:false
      |> TG.add_segment ~xyd1:(3,y,Right) ~xyd2:(5,y,Left) ~dist:2 ~double:false
      |> TG.add_segment ~xyd1:(3,y,UpRight) ~xyd2:(4,y-1,DownLeft) ~dist:1 ~double:false
    in
    print_graph g;
    [%expect {| [[[4,1],[3,2],{"nodes":[[[3,2],["UpRight"]],[[4,1],["DownLeft"]]],"dist":1,"double":false,"block":false}],[[5,2],[3,2],{"nodes":[[[3,2],["Right"]],[[5,2],["Left"]]],"dist":2,"double":false,"block":false}],[[3,2],[1,2],{"nodes":[[[1,2],["Right"]],[[3,2],["Left"]]],"dist":2,"double":false,"block":false}]] |}];
    let g = TG.Track.handle_remove_track g ~x ~y scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"double":false,"block":false}]] |}]

end
