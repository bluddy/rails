open Containers
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
    tmap @@
    Iter.(start -- end_)

let print (segments:SM.t) = SM.yojson_of_t segments |> Yojson.Safe.to_string |> print_string

let player = 0

let build_track (x,y) tmap ~graph ~dirs =
  let before = TS.scan tmap ~x ~y ~player in
  let tmap = TM.set ~x ~y ~t:(make_tm dirs) tmap in
  let after = TS.scan tmap ~x ~y ~player in
  let graph = TG.Track.handle_build_track graph ~x ~y before after in
  tmap

let build_station (x,y) tmap ~graph ~dirs =
  let before = TS.scan tmap ~x ~y ~player in
  let tmap = TM.set ~x ~y ~t:(make_tm dirs ~track:(Station `Depot)) tmap in
  let after = TS.scan tmap ~x ~y ~player in
  let graph = TG.Track.handle_build_station graph ~x ~y before after in
  tmap


(* Test build_station
   graph segment_map trackmap loc scan2
     s
   *)
let%expect_test "build station" =
  let x, y = 10, 10 in
  let graph, segments = TG.make (), SM.make () in
  let tmap = build_road x x tmap
    |> build_station (x,y) ~graph ~dirs:[Left;Right]
  in
  let after = TS.scan tmap ~x ~y ~player in
  let segments = SM.build_station graph segments tmap (x,y) after in
  print segments;
  [%expect {| {"last":2,"counts":[[1,0],[0,0]],"stations":[[[[10,10],["Upper"]],1],[[[10,10],["Lower"]],0]]} |}]

let%expect_test "build station between ixns" =
  let x, y = 10, 10 in
  let graph, segments = TG.make (), SM.make () in
  let tmap = build_road 5 15 tmap
    |> build_track (5,10) ~graph ~dirs:[Left;Right;UpLeft]
    |> build_track (15,10) ~graph ~dirs:[Left;Right;UpRight]
    |> build_station (x, y) ~graph ~dirs:[Left;Right]
  in
  let after = TS.scan tmap ~x ~y ~player in
  let segments = SM.build_station graph segments tmap (x,y) after in
  print segments;
  [%expect {| {"last":2,"counts":[[1,0],[0,0]],"stations":[[[[10,10],["Upper"]],1],[[[10,10],["Lower"]],0]]} |}]


(* Test build_track
   graph trackmap segment_map scan1 scan2
   *)

(* Test remove_track
   graph trackmap segment_map scan1 scan2
   *)

(* Test remove_station
   graph trackmap segment_map loc scan1 scan2
   *)
