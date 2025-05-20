open Containers
open Test_common
module TG = Track_graph
module C = Constants

let graph () =
  TG.make ()
  |> TG.add_ixn (1, 2)
  |> TG.add_ixn (3, 4)
  |> TG.add_segment ~xyd1:(1,2,Dir.Up) ~xyd2:(3,4,Down) ~dist:5

let%expect_test "iter_succ_ixn_dirs" =
  let graph = graph () in
  TG.iter_succ_ixn_dirs (fun _ixn dir ->
    print_string @@ Dir.show dir
  ) graph ~ixn:(1,2);
  [%expect {| Dir.Down |}]

let graph () =
  TG.make ()
  |> TG.add_ixn (1, 2)
  |> TG.add_ixn (3, 4)
  |> TG.add_ixn (5, 6)
  |> TG.add_ixn (7, 8)
  |> TG.add_segment ~xyd1:(1,2,Dir.Up) ~xyd2:(3,4,Right) ~dist:5
  |> TG.add_segment ~xyd1:(1,2,Dir.UpRight) ~xyd2:(5,6,Left) ~dist:10
  |> TG.add_segment ~xyd1:(5,6,Dir.UpRight) ~xyd2:(7,8,DownLeft) ~dist:3

let build_station ((x,y) as loc) ~dirs (tmap, graph) =
  let before = TS.scan tmap loc C.player in
  let tmap = TM.set loc (make_tm dirs ~track:(Station `Depot)) tmap in
  let after = TS.scan tmap loc C.player in
  let graph = TG.Track.handle_build_station x y graph before after in
  tmap, graph

let remove_station ((x,y) as loc) (tmap, graph) =
  let before = TS.scan tmap loc C.player in
  let tmap = TM.remove loc tmap in
  let after = TS.scan tmap loc C.player in
  let graph = TG.Track.handle_remove_track x y graph before after in
  tmap, graph

let print_graph g =
  TG.yojson_of_t g |> Yojson.Safe.to_string |> print_string

let%expect_test "graph print" =
  let g = graph () in
  print_graph g;
  [%expect {| [[[3,4],[1,2],{"nodes":[[[1,2],["Up"]],[[3,4],["Right"]]],"dist":5,"block":false}],[[5,6],[1,2],{"nodes":[[[1,2],["UpRight"]],[[5,6],["Left"]]],"dist":10,"block":false}],[[7,8],[5,6],{"nodes":[[[5,6],["UpRight"]],[[7,8],["DownLeft"]]],"dist":3,"block":false}]] |}]

let%expect_test "graph remove segment" =
  let g = graph ()
    |> TG.remove_segment ~xyd:(1,2,Dir.Up)
  in
  print_graph g;
  [%expect {| [[[5,6],[1,2],{"nodes":[[[1,2],["UpRight"]],[[5,6],["Left"]]],"dist":10,"block":false}],[[7,8],[5,6],{"nodes":[[[5,6],["UpRight"]],[[7,8],["DownLeft"]]],"dist":3,"block":false}]] |}]

let%expect_test "graph find ixn from ixn" =
  let g = graph () in
  let res = TG.find_ixn_from_ixn_dir g ~ixn:(3,4) ~dir:Down in
  Option.pp Utils.pp_loc Format.std_formatter res;
  [%expect {| None |}]

let%expect_test "graph shortest path" =
  let g = graph ()
    |> TG.add_segment ~xyd1:(3,4,UpRight) ~xyd2:(5,6,DownRight) ~dist:8
  in
  let res = TG.shortest_path g ~src:(1,2) ~dest:(5,6) in
  Option.pp Dir.pp Format.std_formatter res;
  [%expect {| Some Dir.UpRight |}]

let%expect_test "graph shortest path" =
  let g = graph ()
    |> TG.add_segment ~xyd1:(3,4,UpRight) ~xyd2:(5,6,DownRight) ~dist:2
  in
  let res = TG.shortest_path g ~src:(1,2) ~dest:(5,6) in
  Option.pp Dir.pp Format.std_formatter res;
  [%expect {| Some Dir.Up |}]

let%expect_test "connected_stations_dirs" =
  let t = Track.make Dir.Set.empty (Station `Station) C.player in
  let map = Trackmap.empty 10 10
    |> Trackmap.set (1, 2) t
    |> Trackmap.set (3, 4) t
    |> Trackmap.set (5, 6) t
  in
  let g = graph () in
  let res = TG.connected_stations_dirs g map [1,2] |> Utils.LocuSet.elements in
  List.pp (Pair.pp (Pair.pp Int.pp Int.pp) Dir.pp_upper) Format.std_formatter res;
  [%expect {|
    5, 6, `Upper, 3, 4,
    `Lower |}]

module Track = struct
  module TM = Trackmap

  let track dirs = 
    Track.make (Dir.Set.of_list dirs) (Track `Single) C.player

  let station dirs = 
    Track.make (Dir.Set.of_list dirs) (Station `Station) C.player

  let dirs = [Dir.Left; Right]
  let y = 2
  (* x---- *)
  let std_map () =
    TM.empty 7 7
    |> TM.set (1,y) (track @@ UpLeft::dirs)
    |> TM.set (2,y) (track dirs)
    |> TM.set (3,y) (track dirs)
    |> TM.set (4,y) (track dirs)
    |> TM.set (5,y) (track [Left])

  let%expect_test "build_station at end of track" =
    (* x---- map *)
    let map = std_map () in
    let scan1 = Scan.scan map (5, y) C.player in
    (* Add station x x---s *)
    let map = TM.set (5, y) (station dirs) map in
    let scan2 = Scan.scan map (5, y) C.player in
    (* Corresponding graph *)
    let g = TG.make ()
      |> TG.add_ixn (1, y)
    in
    print_graph g;
    [%expect {| [] |}];
    let g = TG.Track.handle_build_station 5 y g scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"block":false}]] |}]

  let%expect_test "build_station mid track" =
    (* x---x -> x-s-x *)
    let map = std_map ()
      |> TM.set (5, y) (track @@ UpRight::dirs)
    in
    let scan1 = Scan.scan map (3, y) C.player in
    let map = TM.set (3, y) (station dirs) map in
    let scan2 = Scan.scan map (3, y) C.player in
    (* Corresponding graph *)
    let g = TG.make ()
      |> TG.add_ixn (1, y)
      |> TG.add_ixn (4, y)
      |> TG.add_segment ~xyd1:(1,1,Right) ~xyd2:(5,1,Left) ~dist:5
    in
    print_graph g;
    [%expect {| [[[5,1],[1,1],{"nodes":[[[1,1],["Right"]],[[5,1],["Left"]]],"dist":5,"block":false}]] |}];
    let g = TG.Track.handle_build_station 3 y g scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[3,2],{"nodes":[[[3,2],["Right"]],[[5,2],["Left"]]],"dist":2,"block":false}],[[5,1],[1,1],{"nodes":[[[1,1],["Right"]],[[5,1],["Left"]]],"dist":5,"block":false}],[[3,2],[1,2],{"nodes":[[[1,2],["Right"]],[[3,2],["Left"]]],"dist":2,"block":false}]] |}]

  let%expect_test "build_track_simple" =
    (* x--- x -> x---x *)
    let map = std_map ()
      |> TM.set (4, y) (track [Left])
      |> TM.set (5, y) (track [Left; Right; UpRight])
    in
    let scan1 = Scan.scan map (4, y) C.player in
    let map = TM.set (4, y) (track [Left;Right]) map in
    let scan2 = Scan.scan map (4, y) C.player in
    let g = TG.make () in
    print_graph g;
    [%expect {| [] |}];
    let g = TG.Track.handle_build_track_simple g scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"block":false}]] |}]

    (* same as above, just handle_build_track *)
  let%expect_test "build_track connect ixn" =
    (* x--- x -> x---x *)
    let map = std_map ()
      |> TM.set (4, y) (track [Left])
      |> TM.set (5, y) (track [Left; Right; UpRight])
    in
    let scan1 = Scan.scan map (4, y) C.player in
    let map = TM.set (4, y) (track [Left;Right]) map in
    let scan2 = Scan.scan map (4, y) C.player in
    let g = TG.make () in
    print_graph g;
    [%expect {| [] |}];
    let g = TG.Track.handle_build_track 4 y g scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"block":false}]] |}]

  let%expect_test "build_track create ixn at end" =
    (* x----  -> x---x *)
    let map = std_map () in
    let scan1 = Scan.scan map (5, y) C.player in
    let map = TM.set (5, y) (track [Left; Right; UpRight]) map in
    let scan2 = Scan.scan map (5, y) C.player in
    let g = TG.make () in
    print_graph g;
    [%expect {| [] |}];
    let g = TG.Track.handle_build_track 5 y g scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"block":false}]] |}]

  let%expect_test "build_track create ixn in middle" =
     (* x---x  -> x-x-x *)
    let map = std_map ()
      |> TM.set (5, y) (track [Left; Right; UpRight])
    in
    let scan1 = Scan.scan map (3, y) C.player in
    let map = TM.set (3, y) (track [Left; Right; UpRight]) map in
    let scan2 = Scan.scan map (3, y) C.player in
    let g = TG.make ()
      |> TG.add_segment ~xyd1:(1,y,Right) ~xyd2:(5,y,Left) ~dist:4
    in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"block":false}]] |}];
    let g = TG.Track.handle_build_track 3 y g scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[3,2],{"nodes":[[[3,2],["Right"]],[[5,2],["Left"]]],"dist":2,"block":false}],[[3,2],[1,2],{"nodes":[[[1,2],["Right"]],[[3,2],["Left"]]],"dist":2,"block":false}]] |}]

  let%expect_test "build_track create ixn in middle (with stations)" =
     (* s---s  -> s-x-s *)
    let map = std_map ()
      |> TM.set (1, y) (station [Left; Right])
      |> TM.set (5, y) (station [Left; Right])
    in
    let scan1 = Scan.scan map (3, y) C.player in
    let map = TM.set (3, y) (track [Left; Right; UpRight]) map in
    let scan2 = Scan.scan map (3, y) C.player in
    let g = TG.make ()
      |> TG.add_segment ~xyd1:(1,y,Right) ~xyd2:(5,y,Left) ~dist:4
    in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"block":false}]] |}];
    let g = TG.Track.handle_build_track 3 y g scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[3,2],{"nodes":[[[3,2],["Right"]],[[5,2],["Left"]]],"dist":2,"block":false}],[[3,2],[1,2],{"nodes":[[[1,2],["Right"]],[[3,2],["Left"]]],"dist":2,"block":false}]] |}]

  let%expect_test "build_track create+connect to another ixn" =
     (*    x          x
          /          /
       x---x  -> x-x-x *)
    let map = std_map ()
      |> TM.set (5, y) (track [Left; Right; UpRight])
      |> TM.set (4, y-1) (track [DownLeft; UpRight])
      |> TM.set (5, y-2) (station [DownLeft; UpRight])
    in
    let scan1 = Scan.scan map (3, y) C.player in
    let map = TM.set (3, y) (track [Left; Right; UpRight]) map in
    let scan2 = Scan.scan map (3, y) C.player in
    let g = TG.make ()
      |> TG.add_segment ~xyd1:(1,y,Right) ~xyd2:(5,y,Left) ~dist:4
    in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"block":false}]] |}];
    let g = TG.Track.handle_build_track 3 y g scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[3,2],{"nodes":[[[3,2],["Right"]],[[5,2],["Left"]]],"dist":2,"block":false}],[[5,0],[3,2],{"nodes":[[[3,2],["UpRight"]],[[5,0],["DownLeft"]]],"dist":2,"block":false}],[[3,2],[1,2],{"nodes":[[[1,2],["Right"]],[[3,2],["Left"]]],"dist":2,"block":false}]] |}]

  let%expect_test "remove_track full" = 
     (* x---x  -> x- -x *)
    let map = std_map ()
      |> TM.set (5, y) (track [Left; Right; UpRight])
    in
    let scan1 = Scan.scan map (3, y) C.player in
    let map = TM.remove (3, y) map in
    let scan2 = Scan.scan map (3, y) C.player in
    let g = TG.make ()
      |> TG.add_segment ~xyd1:(1,y,Right) ~xyd2:(5,y,Left) ~dist:4
    in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"block":false}]] |}];
    let g = TG.Track.handle_remove_track 3 y g scan1 scan2 in
    print_graph g;
    [%expect {| [] |}]

  let%expect_test "remove_track partial" = 
     (* x---x  -> x- -x *)
    let map = std_map ()
      |> TM.set (5, y) (track [Left; Right; UpRight])
    in
    let scan1 = Scan.scan map (3, y) C.player in
    let map = TM.set (3, y) (track [Left]) map in
    let scan2 = Scan.scan map (3, y) C.player in
    let g = TG.make ()
      |> TG.add_segment ~xyd1:(1,y,Right) ~xyd2:(5,y,Left) ~dist:4
    in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"block":false}]] |}];
    let g = TG.Track.handle_remove_track 3 y g scan1 scan2 in
    print_graph g;
    [%expect {| [] |}]

  let%expect_test "remove station" = 
     (* x---S  -> x--- *)
    let map = std_map ()
      |> TM.set (5, y) (station [Left; Right])
    in
    let scan1 = Scan.scan map (5, y) C.player in
    let map = TM.set (5, y) (track [Left]) map in
    let scan2 = Scan.scan map (5, y) C.player in
    let g = TG.make ()
      |> TG.add_segment ~xyd1:(1,y,Right) ~xyd2:(5,y,Left) ~dist:4
    in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"block":false}]] |}];
    let g = TG.Track.handle_remove_track 5 y g scan1 scan2 in
    print_graph g;
    [%expect {| [] |}]

  let%expect_test "remove ixn" = 
     (* x---x  -> x--- *)
    let map = std_map ()
      |> TM.set (5, y) (station [Left; Right; UpRight])
    in
    let x = 5 in
    let scan1 = Scan.scan map (x, y) C.player in
    let map = TM.set (x, y) (track [Left]) map in
    let scan2 = Scan.scan map (x, y) C.player in
    let g = TG.make ()
      |> TG.add_segment ~xyd1:(1,y,Right) ~xyd2:(x,y,Left) ~dist:4
    in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"block":false}]] |}];
    let g = TG.Track.handle_remove_track x y g scan1 scan2 in
    print_graph g;
    [%expect {| [] |}]

  let%expect_test "remove ixn in middle" = 
     (* x-x-x  -> x---x*)
    let map = std_map ()
      |> TM.set (5, y) (track [Left; Right; UpRight])
      |> TM.set (3, y) (track [Left; Right; UpRight])
    in
    let x = 3 in
    let scan1 = Scan.scan map (x, y) C.player in
    let map = TM.set (x, y) (track [Left;Right]) map in
    let scan2 = Scan.scan map (x, y) C.player in
    let g = TG.make ()
      |> TG.add_segment ~xyd1:(1,y,Right) ~xyd2:(3,y,Left) ~dist:2
      |> TG.add_segment ~xyd1:(3,y,Right) ~xyd2:(5,y,Left) ~dist:2
    in
    print_graph g;
    [%expect {| [[[5,2],[3,2],{"nodes":[[[3,2],["Right"]],[[5,2],["Left"]]],"dist":2,"block":false}],[[3,2],[1,2],{"nodes":[[[1,2],["Right"]],[[3,2],["Left"]]],"dist":2,"block":false}]] |}];
    let g = TG.Track.handle_remove_track x y g scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"block":false}]] |}]

  let%expect_test "remove ixn in middle" = 
     (*  x         x
       x-x-x  -> x---x*)
    let map = std_map ()
      |> TM.set (5, y) (track [Left; Right; UpRight])
      |> TM.set (3, y) (track [Left; Right; UpRight])
      |> TM.set (4, y-1) (track [DownLeft; Up; UpRight])
    in
    let x = 3 in
    let scan1 = Scan.scan map (x, y) C.player in
    let map = TM.set (x, y) (track [Left;Right]) map in
    let scan2 = Scan.scan map (x, y) C.player in
    let g = TG.make ()
      |> TG.add_segment ~xyd1:(1,y,Right) ~xyd2:(3,y,Left) ~dist:2
      |> TG.add_segment ~xyd1:(3,y,Right) ~xyd2:(5,y,Left) ~dist:2
      |> TG.add_segment ~xyd1:(3,y,UpRight) ~xyd2:(4,y-1,DownLeft) ~dist:1
    in
    print_graph g;
    [%expect {| [[[4,1],[3,2],{"nodes":[[[3,2],["UpRight"]],[[4,1],["DownLeft"]]],"dist":1,"block":false}],[[5,2],[3,2],{"nodes":[[[3,2],["Right"]],[[5,2],["Left"]]],"dist":2,"block":false}],[[3,2],[1,2],{"nodes":[[[1,2],["Right"]],[[3,2],["Left"]]],"dist":2,"block":false}]] |}];
    let g = TG.Track.handle_remove_track x y g scan1 scan2 in
    print_graph g;
    [%expect {| [[[5,2],[1,2],{"nodes":[[[1,2],["Right"]],[[5,2],["Left"]]],"dist":4,"block":false}]] |}]

  let%expect_test "4 connected stations in a square, disconnect all" =
    let tm, tg = (square_track (), TG.make ())
     |> build_station (6, 5) ~dirs:[Left; Right] in
    print_graph tg;
    [%expect {| [[[6,5],[6,5],{"nodes":[[[6,5],["Right"]],[[6,5],["Left"]]],"dist":40,"block":false}]] |}];
    let tm, tg = build_station (14, 5) ~dirs:[Left; Right] (tm, tg) in
    print_graph tg;
    [%expect{| [[[14,5],[6,5],{"nodes":[[[6,5],["Left"]],[[14,5],["Right"]]],"dist":32,"block":false}],[[14,5],[6,5],{"nodes":[[[6,5],["Right"]],[[14,5],["Left"]]],"dist":8,"block":false}]] |}];
    let tm, tg = build_station (14, 15) ~dirs:[Left; Right] (tm, tg) in
    print_graph tg;
    [%expect{| [[[14,5],[6,5],{"nodes":[[[6,5],["Right"]],[[14,5],["Left"]]],"dist":8,"block":false}],[[14,15],[14,5],{"nodes":[[[14,5],["Right"]],[[14,15],["Right"]]],"dist":12,"block":false}],[[14,15],[6,5],{"nodes":[[[6,5],["Left"]],[[14,15],["Left"]]],"dist":20,"block":false}]] |}];
    let tm, tg = build_station (6, 15) ~dirs:[Left; Right] (tm, tg) in
    print_graph tg;
    [%expect{| [[[6,15],[6,5],{"nodes":[[[6,5],["Left"]],[[6,15],["Left"]]],"dist":12,"block":false}],[[14,5],[6,5],{"nodes":[[[6,5],["Right"]],[[14,5],["Left"]]],"dist":8,"block":false}],[[14,15],[14,5],{"nodes":[[[14,5],["Right"]],[[14,15],["Right"]]],"dist":12,"block":false}],[[14,15],[6,15],{"nodes":[[[6,15],["Right"]],[[14,15],["Left"]]],"dist":8,"block":false}]] |}];
    let _tm, tg = remove_station (14, 15) (tm, tg) in
    print_graph tg;
    [%expect{| [[[6,15],[6,5],{"nodes":[[[6,5],["Left"]],[[6,15],["Left"]]],"dist":12,"block":false}],[[14,5],[6,5],{"nodes":[[[6,5],["Right"]],[[14,5],["Left"]]],"dist":8,"block":false}]] |}];
    let _tm, tg = remove_station (6, 15) (tm, tg) in
    print_graph tg;
    [%expect{| [[[14,5],[6,5],{"nodes":[[[6,5],["Right"]],[[14,5],["Left"]]],"dist":8,"block":false}]] |}];
    let _tm, tg = remove_station (6, 5) (tm, tg) in
    print_graph tg;
    [%expect{| [] |}];
    ()
end
