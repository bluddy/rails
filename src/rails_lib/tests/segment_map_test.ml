open! Containers
open Track
module TM = Trackmap
module SM = Segment_map
module TG = Track_graph
module TS = TM.Search

let make_tm ?(track=Track `Single) dirs = 
  Track.make (Dir.Set.of_list dirs) track ~player:0

let tmap = TM.empty 20 20

let build_road start end_ map =
  Iter.fold
    (fun acc x -> TM.set acc ~x ~y:10 ~t:(make_tm [Left;Right]))
    map @@
    Iter.(start -- end_)

let print (segments:SM.t) = SM.yojson_of_t segments |> Yojson.Safe.to_string |> print_string

let player = 0

let build_track (x,y) (tmap, graph) ~dirs =
  let before = TS.scan tmap ~x ~y ~player in
  let tmap = TM.set ~x ~y ~t:(make_tm dirs) tmap in
  let after = TS.scan tmap ~x ~y ~player in
  let graph = TG.Track.handle_build_track graph ~x ~y before after in
  tmap, graph

let build_station (x,y) tmap ~graph ~dirs =
  let before = TS.scan tmap ~x ~y ~player in
  let tmap = TM.set ~x ~y ~t:(make_tm dirs ~track:(Station `Depot)) tmap in
  let after = TS.scan tmap ~x ~y ~player in
  let graph = TG.Track.handle_build_station graph ~x ~y before after in
  tmap, after

(* Test build_station
   graph segment_map trackmap loc scan2
     s
   *)
let%expect_test "build station" =
  let x, y = 10, 10 in
  let graph, segments = TG.make (), SM.make () in
  let tmap, after = build_road x x tmap
    |> build_station (x,y) ~graph ~dirs:[Left;Right]
  in
  let segments = SM.build_station graph segments tmap (x,y) after in
  print segments;
  [%expect {| {"last":2,"counts":[[1,0],[0,0]],"stations":[[[[10,10],["Upper"]],1],[[[10,10],["Lower"]],0]]} |}]

let%expect_test "build station between ixns" =
  let x, y = 10, 10 in
  let graph, segments = TG.make (), SM.make () in
  let tmap, after =
    let tmap = build_road 5 15 tmap in
    let tmap, graph = 
      build_track (5,10) ~dirs:[Left;Right;UpLeft] (tmap, graph)
      |> build_track (15,10) ~dirs:[Left;Right;UpRight]
    in
    build_station (x, y) ~dirs:[Left;Right] tmap ~graph
  in
  let segments = SM.build_station graph segments tmap (x,y) after in
  print segments;
  [%expect {| {"last":2,"counts":[[1,0],[0,0]],"stations":[[[[10,10],["Upper"]],1],[[[10,10],["Lower"]],0]]} |}]

let%expect_test "build second station" =
  let graph, segments = TG.make (), SM.make () in
  let tmap, after = build_road 5 15 tmap
    |> build_station (10, 10) ~graph ~dirs:[Left;Right]
  in
  let segments = SM.build_station graph segments tmap (10,10) after in
  let tmap, after = build_station (5, 10) ~graph ~dirs:[Left;Right] tmap in
  let segments = SM.build_station graph segments tmap (5,10) after in
  print segments;
  [%expect {| {"last":3,"counts":[[1,0],[0,0],[2,0]],"stations":[[[[10,10],["Upper"]],1],[[[5,10],["Upper"]],2],[[[5,10],["Lower"]],1],[[[10,10],["Lower"]],0]]} |}]

let build_station_seg loc ~graph ~dirs (tmap, segments) =
  let tmap, after = build_station loc ~graph ~dirs tmap in
  let segments = SM.build_station graph segments tmap loc after in
  tmap, segments

let dirs = Dir.[Left; Right]

let%expect_test "build 3 stations left to right " =
  let graph, segments = TG.make (), SM.make () in
  let tmap = build_road 5 15 tmap in
  let _, segments =
    (tmap, segments)
    |> build_station_seg (5,10) ~graph ~dirs
    |> build_station_seg (10,10) ~graph ~dirs
    |> build_station_seg (15,10) ~graph ~dirs
  in
  print segments;
  [%expect {| {"last":4,"counts":[[1,0],[0,0],[3,0],[2,0]],"stations":[[[[10,10],["Upper"]],0],[[[5,10],["Upper"]],1],[[[5,10],["Lower"]],0],[[[10,10],["Lower"]],2],[[[15,10],["Lower"]],3],[[[15,10],["Upper"]],2]]} |}]

  (* TODO: bug. We don't split properly when building a station *)
let%expect_test "build 2 stations and then one in the middle" =
  let graph, segments = TG.make (), SM.make () in
  let tmap = build_road 5 15 tmap in
  let tmap, segments =
    (tmap, segments)
    |> build_station_seg (5,10) ~graph ~dirs
    |> build_station_seg (15,10) ~graph ~dirs
  in
  print segments;
  [%expect {| {"last":3,"counts":[[1,0],[0,0],[2,0]],"stations":[[[[5,10],["Upper"]],1],[[[5,10],["Lower"]],0],[[[15,10],["Lower"]],2],[[[15,10],["Upper"]],0]]} |}];
  (* Now the middle station *)
  let _, segments =
    build_station_seg (10,10) ~graph ~dirs (tmap, segments) in
  print segments;
  [%expect {| {"last":4,"counts":[[1,0],[0,0],[3,0],[2,0]],"stations":[[[[10,10],["Upper"]],3],[[[5,10],["Upper"]],1],[[[5,10],["Lower"]],3],[[[10,10],["Lower"]],0],[[[15,10],["Lower"]],2],[[[15,10],["Upper"]],3]]} |}]

(* build 2 stations separated by ixn *)
let%expect_test "build 2 stations separated by ixn" =
  let graph, segments = TG.make (), SM.make () in
  let tmap = build_road 5 15 tmap in
  ()

(* build 3 stations separated by ixns *)



(* Test build_track
   graph trackmap segment_map scan1 scan2
   *)

(* Test remove_track
   graph trackmap segment_map scan1 scan2
   *)

(* Test remove_station
   graph trackmap segment_map loc scan1 scan2
   *)
