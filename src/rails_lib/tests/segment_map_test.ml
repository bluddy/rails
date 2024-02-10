open! Containers
open Track
module TM = Trackmap
module SM = Segment_map
module TG = Track_graph
module TS = Scan

let make_tm ?(track=Track `Single) dirs = 
  Track.make (Dir.Set.of_list dirs) track ~player:0

let tmap = TM.empty 20 20

let build_road start end_ map =
  Iter.fold
    (fun acc x -> TM.set acc ~x ~y:10 ~t:(make_tm [Left;Right]))
    map @@
    Iter.(start -- end_)

let print (segments:SM.t) = SM.yojson_of_t segments |> Yojson.Safe.to_string |> print_string

let print_graph g = TG.yojson_of_t g |> Yojson.Safe.to_string |> print_string

let trainmap = Trainmap.empty ()

let player = 0

let build_track (x,y) (tmap, graph, segments) ~dirs =
  let before = TS.scan tmap ~x ~y ~player in
  let tmap = TM.set ~x ~y ~t:(make_tm dirs) tmap in
  let after = TS.scan tmap ~x ~y ~player in
  let graph = TG.Track.handle_build_track graph ~x ~y before after in
  let segments = SM.handle_build_track graph tmap trainmap segments before after in
  tmap, graph, segments

let remove_track (x,y) (tmap, graph, segments) =
  let before = TS.scan tmap ~x ~y ~player in
  let tmap = TM.remove ~x ~y tmap in
  let after = TS.scan tmap ~x ~y ~player in
  let graph = TG.Track.handle_remove_track graph ~x ~y before after in
  let segments = SM.handle_remove_track graph tmap trainmap segments before after in
  tmap, graph, segments

let build_station loc ~dirs (tmap, graph, segments) =
  let build_station_inner (x,y) tmap ~graph ~dirs =
    let before = TS.scan tmap ~x ~y ~player in
    let tmap = TM.set ~x ~y ~t:(make_tm dirs ~track:(Station `Depot)) tmap in
    let after = TS.scan tmap ~x ~y ~player in
    let graph = TG.Track.handle_build_station graph ~x ~y before after in
    tmap, graph, after
  in
  let tmap, graph, after = build_station_inner loc ~graph ~dirs tmap in
  let segments = SM.handle_build_station graph segments tmap trainmap loc after in
  tmap, graph, segments

let remove_station (x,y) (tmap, graph, segments) =
  let before = TS.scan tmap ~x ~y ~player in
  let segments = SM.handle_remove_station graph tmap segments (x,y) before in
  let tmap = TM.remove ~x ~y tmap in
  let after = TS.scan tmap ~x ~y ~player in
  let graph = TG.Track.handle_remove_track graph ~x ~y before after in
  tmap, graph, segments

(* Test build_station
   graph segment_map trackmap loc scan2
     s
   *)
let%expect_test "build station" =
  let loc = 10, 10 in
  let graph, segments = TG.make (), SM.make () in
  let (_, _, segments) =
    build_track loc (tmap, graph, segments) ~dirs:[Left; Right]
    |> build_station loc ~dirs:[Left; Right]
  in
  print segments;
  [%expect {| {"info":[[1,{"count":0,"double":["Double"]}],[0,{"count":0,"double":["Double"]}]],"stations":[[[[10,10],["Upper"]],0],[[[10,10],["Lower"]],1]]} |}]

let%expect_test "build station between ixns" =
  let graph, segments = TG.make (), SM.make () in
  let tmap = build_road 5 15 tmap in
  let _, _, segments = 
    (tmap, graph, segments)
    |> build_track (5,10) ~dirs:[Left;Right;UpLeft]
    |> build_track (15,10) ~dirs:[Left;Right;UpRight]
    |> build_station (10, 10) ~dirs:[Left;Right]
  in
  print segments;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  Not_found
  Raised at Stdlib__Hashtbl.find in file "hashtbl.ml", line 542, characters 13-28
  Called from Rails_lib__Segment_map.get_station_seg in file "src/rails_lib/backend/segment_map.ml" (inlined), line 65, characters 2-30
  Called from Rails_lib__Segment_map.handle_build_station in file "src/rails_lib/backend/segment_map.ml", line 168, characters 20-46
  Called from Rails_lib__Segment_map_test.build_station in file "src/rails_lib/tests/segment_map_test.ml", line 52, characters 17-79
  Called from Rails_lib__Segment_map_test.(fun) in file "src/rails_lib/tests/segment_map_test.ml", line 81, characters 4-182
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]

let%expect_test "build second station" =
  let graph, segments = TG.make (), SM.make () in
  let dirs = Dir.[Left; Right] in
  let tmap = build_road 5 15 tmap in
  let _, _, segments =
    (tmap, graph, segments)
    |> build_station (10, 10) ~dirs
    |> build_station (5, 10) ~dirs
  in
  print segments;
  [%expect {| {"info":[[1,{"count":0,"double":["Double"]}],[0,{"count":0,"double":["Single"]}],[2,{"count":0,"double":["Double"]}]],"stations":[[[[10,10],["Upper"]],0],[[[5,10],["Upper"]],2],[[[5,10],["Lower"]],0],[[[10,10],["Lower"]],1]]} |}]

let%expect_test "build 3 stations left to right " =
  let graph, segments = TG.make (), SM.make () in
  let dirs = Dir.[Left; Right] in
  let tmap = build_road 5 15 tmap in
  let _, _, segments =
    (tmap, graph, segments)
    |> build_station (5,10) ~dirs
    |> build_station (10,10) ~dirs
    |> build_station (15,10) ~dirs
  in
  print segments;
  [%expect {| {"info":[[1,{"count":0,"double":["Single"]}],[0,{"count":0,"double":["Double"]}],[3,{"count":0,"double":["Double"]}],[2,{"count":0,"double":["Single"]}]],"stations":[[[[10,10],["Upper"]],1],[[[5,10],["Upper"]],0],[[[5,10],["Lower"]],1],[[[10,10],["Lower"]],2],[[[15,10],["Lower"]],3],[[[15,10],["Upper"]],2]]} |}]

let%expect_test "build 2 stations and then one in the middle" =
  let graph, segments = TG.make (), SM.make () in
  let dirs = Dir.[Left; Right] in
  let tmap = build_road 5 15 tmap in
  let tgs =
    (tmap, graph, segments)
    |> build_station (5,10) ~dirs
    |> build_station (15,10) ~dirs
  in
  print_graph graph;
  [%expect {| [[[15,10],[5,10],{"nodes":[[[5,10],["Right"]],[[15,10],["Left"]]],"dist":10,"block":false}]] |}];
  print segments;
  [%expect {| {"info":[[1,{"count":0,"double":["Single"]}],[0,{"count":0,"double":["Double"]}],[2,{"count":0,"double":["Double"]}]],"stations":[[[[5,10],["Upper"]],0],[[[5,10],["Lower"]],1],[[[15,10],["Lower"]],2],[[[15,10],["Upper"]],1]]} |}];
  (* Now the middle station *)
  let _, graph, segments =
    build_station (10,10) ~dirs tgs in
  print_graph graph;
  [%expect {| [[[10,10],[5,10],{"nodes":[[[5,10],["Right"]],[[10,10],["Left"]]],"dist":5,"block":false}],[[15,10],[10,10],{"nodes":[[[10,10],["Right"]],[[15,10],["Left"]]],"dist":5,"block":false}]] |}];
  print segments;
  [%expect {| {"info":[[1,{"count":0,"double":["Single"]}],[0,{"count":0,"double":["Double"]}],[3,{"count":0,"double":["Single"]}],[2,{"count":0,"double":["Double"]}]],"stations":[[[[10,10],["Upper"]],1],[[[5,10],["Upper"]],0],[[[5,10],["Lower"]],1],[[[10,10],["Lower"]],3],[[[15,10],["Lower"]],2],[[[15,10],["Upper"]],3]]} |}]

(* build 2 stations separated by ixn *)
let%expect_test "build 2 stations separated by ixn" =
  let graph, segments = TG.make (), SM.make () in
  let dirs = Dir.[Left; Right] in
  let tmap = build_road 5 15 tmap in
  let _, _, segments =
    (tmap, graph, segments)
    |> build_station (5,10) ~dirs
    |> build_station (15,10) ~dirs
    |> build_track (10,10) ~dirs:[Left;Right;UpRight]
  in
  print segments;
  [%expect {| {"info":[[1,{"count":0,"double":["Single"]}],[0,{"count":0,"double":["Double"]}],[2,{"count":0,"double":["Double"]}]],"stations":[[[[5,10],["Upper"]],0],[[[5,10],["Lower"]],1],[[[15,10],["Lower"]],2],[[[15,10],["Upper"]],1]]} |}]


(* Test build_track
   graph trackmap segment_map scan1 scan2
   *)
let%expect_test "connect 2 station with road" =
  let graph, segments = TG.make (), SM.make () in
  let tmap = build_road 5 15 tmap
    |> TM.remove_track ~x:10 ~y:10 ~dir:Right ~player:0
  in
  let tmap, graph, segments =
    (tmap, graph, segments)
    |> build_station (5, 10) ~dirs:[Left; Right]
    |> build_station (15, 10) ~dirs:[Left; Right]
  in
  print segments;
  [%expect {| {"info":[[1,{"count":0,"double":["Double"]}],[0,{"count":0,"double":["Double"]}],[3,{"count":0,"double":["Double"]}],[2,{"count":0,"double":["Double"]}]],"stations":[[[[5,10],["Upper"]],0],[[[5,10],["Lower"]],1],[[[15,10],["Lower"]],3],[[[15,10],["Upper"]],2]]} |}];
  let _, _, segments =
    build_track (10, 10) (tmap, graph, segments) ~dirs:[Left;Right]
  in
  print segments;
  [%expect {| {"info":[[1,{"count":0,"double":["Single"]}],[0,{"count":0,"double":["Double"]}],[3,{"count":0,"double":["Double"]}]],"stations":[[[[5,10],["Upper"]],0],[[[5,10],["Lower"]],1],[[[15,10],["Lower"]],3],[[[15,10],["Upper"]],1]]} |}]


(* Test remove_track
   graph trackmap segment_map scan1 scan2
   *)
let%expect_test "2 connected stations, disconnect road" =
  let graph, segments = TG.make (), SM.make () in
  let tmap = build_road 5 15 tmap in
  let tmap, graph, segments =
    (tmap, graph, segments)
    |> build_station (5, 10) ~dirs:[Left; Right]
    |> build_station (15, 10) ~dirs:[Left; Right]
  in
  print segments;
  [%expect {| {"info":[[1,{"count":0,"double":["Single"]}],[0,{"count":0,"double":["Double"]}],[2,{"count":0,"double":["Double"]}]],"stations":[[[[5,10],["Upper"]],0],[[[5,10],["Lower"]],1],[[[15,10],["Lower"]],2],[[[15,10],["Upper"]],1]]} |}];
  let _, _, segments = remove_track (10, 10) (tmap, graph, segments) in
  print segments;
  [%expect {| {"info":[[1,{"count":0,"double":["Single"]}],[0,{"count":0,"double":["Double"]}],[2,{"count":0,"double":["Double"]}]],"stations":[[[[5,10],["Upper"]],0],[[[5,10],["Lower"]],1],[[[15,10],["Lower"]],2],[[[15,10],["Upper"]],1]]} |}]

(* Test remove_station
   graph trackmap segment_map loc scan1 scan2
   *)
let%expect_test "2 connected stations, disconnect one" =
  let graph, segments = TG.make (), SM.make () in
  let tmap = build_road 5 15 tmap in
  let tmap, graph, segments =
    (tmap, graph, segments)
    |> build_station (5, 10) ~dirs:[Left; Right]
    |> build_station (15, 10) ~dirs:[Left; Right]
  in
  print segments;
  [%expect {| {"info":[[1,{"count":0,"double":["Single"]}],[0,{"count":0,"double":["Double"]}],[2,{"count":0,"double":["Double"]}]],"stations":[[[[5,10],["Upper"]],0],[[[5,10],["Lower"]],1],[[[15,10],["Lower"]],2],[[[15,10],["Upper"]],1]]} |}];
  let _, _, segments =
    remove_station (5, 10) (tmap, graph, segments) in
  print segments;
  [%expect {| {"info":[[1,{"count":0,"double":["Single"]}],[2,{"count":0,"double":["Double"]}]],"stations":[[[[15,10],["Lower"]],2],[[[15,10],["Upper"]],1]]} |}]

