open Track
module TM = Trackmap
module SM = Segment_map
module TG = Track_graph
module TS = TM.Search

let make_tm ?(track=Track `Single) dirs = 
  Track.make (Dir.Set.of_list dirs) track ~player:0

let tmap = TM.empty 20 20
  |> TM.set ~x:10 ~y:10 ~t:(make_tm [Left; Right])

let print (segments:SM.t) = SM.yojson_of_t segments |> Yojson.Safe.to_string |> print_string

(* Test build_station
   graph segment_map trackmap loc scan2
   *)
let%expect_test "build station" =
  let player = 0 in
  let x, y = 10, 10 in
  let graph = TG.make () in
  let segments = SM.make () in
  let before = TS.scan tmap ~x ~y ~player in
  let tmap, _ = TM.build_station tmap ~x ~y `Depot in
  let after = TS.scan tmap ~x ~y ~player in
  let graph = TG.Track.handle_build_station graph ~x ~y before after in
  let _ = SM.build_station graph segments tmap (x,y) after in
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
