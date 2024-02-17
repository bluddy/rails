open! Containers
open Track
module TM = Trackmap
module SM = Segment_map
module TG = Track_graph
module TS = Scan
module TRM = Trainmap
open Test_common

let print (segments:SM.t) = SM.show segments |> print_string

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
  [%expect {|
    { Segment_map.info = 0 -> { Segment_map.count = 0; double = `Double }, 1
      -> { Segment_map.count = 0; double = `Double };
      stations = ((10, 10), `Lower) -> 1, ((10, 10), `Upper) -> 0 } |}]

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
  [%expect{|
    { Segment_map.info = 0 -> { Segment_map.count = 0; double = `Double }, 1
      -> { Segment_map.count = 0; double = `Double };
      stations = ((10, 10), `Lower) -> 1, ((10, 10), `Upper) -> 0 } |}]

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
  [%expect {|
    { Segment_map.info = 2 -> { Segment_map.count = 0; double = `Double }, 0
      -> { Segment_map.count = 0; double = `Single }, 1
      -> { Segment_map.count = 0; double = `Double };
      stations = ((10, 10), `Lower) -> 1, ((5, 10), `Lower) -> 0,
      ((5, 10), `Upper) -> 2, ((10, 10), `Upper) -> 0 } |}]

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
  [%expect {|
    { Segment_map.info = 2 -> { Segment_map.count = 0; double = `Single }, 3
      -> { Segment_map.count = 0; double = `Double }, 0
      -> { Segment_map.count = 0; double = `Double }, 1
      -> { Segment_map.count = 0; double = `Single };
      stations = ((15, 10), `Upper) -> 2, ((15, 10), `Lower) -> 3,
      ((10, 10), `Lower) -> 2, ((5, 10), `Lower) -> 1, ((5, 10), `Upper) -> 0,
      ((10, 10), `Upper) -> 1 } |}]

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
  [%expect {|
    { Segment_map.info = 2 -> { Segment_map.count = 0; double = `Double }, 0
      -> { Segment_map.count = 0; double = `Double }, 1
      -> { Segment_map.count = 0; double = `Single };
      stations = ((15, 10), `Upper) -> 1, ((15, 10), `Lower) -> 2,
      ((5, 10), `Lower) -> 1, ((5, 10), `Upper) -> 0 } |}];
  (* Now the middle station *)
  let _, graph, segments =
    build_station (10,10) ~dirs tgs in
  print_graph graph;
  [%expect {| [[[10,10],[5,10],{"nodes":[[[5,10],["Right"]],[[10,10],["Left"]]],"dist":5,"block":false}],[[15,10],[10,10],{"nodes":[[[10,10],["Right"]],[[15,10],["Left"]]],"dist":5,"block":false}]] |}];
  print segments;
  [%expect {|
    { Segment_map.info = 2 -> { Segment_map.count = 0; double = `Double }, 3
      -> { Segment_map.count = 0; double = `Single }, 0
      -> { Segment_map.count = 0; double = `Double }, 1
      -> { Segment_map.count = 0; double = `Single };
      stations = ((15, 10), `Upper) -> 3, ((15, 10), `Lower) -> 2,
      ((10, 10), `Lower) -> 3, ((5, 10), `Lower) -> 1, ((5, 10), `Upper) -> 0,
      ((10, 10), `Upper) -> 1 } |}]

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
  [%expect {|
    { Segment_map.info = 2 -> { Segment_map.count = 0; double = `Double }, 0
      -> { Segment_map.count = 0; double = `Double }, 1
      -> { Segment_map.count = 0; double = `Single };
      stations = ((15, 10), `Upper) -> 1, ((15, 10), `Lower) -> 2,
      ((5, 10), `Lower) -> 1, ((5, 10), `Upper) -> 0 } |}]


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
  [%expect {|
    { Segment_map.info = 2 -> { Segment_map.count = 0; double = `Double }, 3
      -> { Segment_map.count = 0; double = `Double }, 0
      -> { Segment_map.count = 0; double = `Double }, 1
      -> { Segment_map.count = 0; double = `Double };
      stations = ((15, 10), `Upper) -> 2, ((15, 10), `Lower) -> 3,
      ((5, 10), `Lower) -> 1, ((5, 10), `Upper) -> 0 } |}];
  let _, _, segments =
    build_track (10, 10) (tmap, graph, segments) ~dirs:[Left;Right]
  in
  print segments;
  [%expect {|
    { Segment_map.info = 3 -> { Segment_map.count = 0; double = `Double }, 0
      -> { Segment_map.count = 0; double = `Double }, 1
      -> { Segment_map.count = 0; double = `Single };
      stations = ((15, 10), `Upper) -> 1, ((15, 10), `Lower) -> 3,
      ((5, 10), `Lower) -> 1, ((5, 10), `Upper) -> 0 } |}]


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
  [%expect {|
    { Segment_map.info = 2 -> { Segment_map.count = 0; double = `Double }, 0
      -> { Segment_map.count = 0; double = `Double }, 1
      -> { Segment_map.count = 0; double = `Single };
      stations = ((15, 10), `Upper) -> 1, ((15, 10), `Lower) -> 2,
      ((5, 10), `Lower) -> 1, ((5, 10), `Upper) -> 0 } |}];
  let _, _, segments = remove_track (10, 10) (tmap, graph, segments) in
  print segments;
  [%expect {|
    { Segment_map.info = 2 -> { Segment_map.count = 0; double = `Double }, 3
      -> { Segment_map.count = 0; double = `Double }, 0
      -> { Segment_map.count = 0; double = `Double }, 1
      -> { Segment_map.count = 0; double = `Single };
      stations = ((15, 10), `Upper) -> 3, ((15, 10), `Lower) -> 2,
      ((5, 10), `Lower) -> 1, ((5, 10), `Upper) -> 0 } |}]

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
  [%expect {|
    { Segment_map.info = 2 -> { Segment_map.count = 0; double = `Double }, 0
      -> { Segment_map.count = 0; double = `Double }, 1
      -> { Segment_map.count = 0; double = `Single };
      stations = ((15, 10), `Upper) -> 1, ((15, 10), `Lower) -> 2,
      ((5, 10), `Lower) -> 1, ((5, 10), `Upper) -> 0 } |}];
  let _, _, segments =
    remove_station (5, 10) (tmap, graph, segments) in
  print segments;
  [%expect {|
    { Segment_map.info = 2 -> { Segment_map.count = 0; double = `Double }, 1
      -> { Segment_map.count = 0; double = `Single };
      stations = ((15, 10), `Upper) -> 1, ((15, 10), `Lower) -> 2 } |}]

let%expect_test "4 connected stations in a square, disconnect one" =
  let graph, segments = TG.make (), SM.make () in
  (* Draw square *)
  let tmap = square_track () in
  let tgs =
    (tmap, graph, segments)
    |> build_station (6, 5) ~dirs:[Left; Right]
  in
  print @@ Utils.thd3 tgs;
  [%expect{|
    { Segment_map.info = 0 -> { Segment_map.count = 0; double = `Single };
      stations = ((6, 5), `Lower) -> 0, ((6, 5), `Upper) -> 0 } |}];

  let tgs = build_station (14, 5) ~dirs:[Left; Right] tgs in
  print @@ Utils.thd3 tgs;
  [%expect {|
    { Segment_map.info = 0 -> { Segment_map.count = 0; double = `Single }, 1
      -> { Segment_map.count = 0; double = `Single };
      stations = ((14, 5), `Lower) -> 1, ((14, 5), `Upper) -> 0, ((6, 5), `Lower)
      -> 0, ((6, 5), `Upper) -> 1 } |}];

  let tgs = build_station (14, 15) ~dirs:[Left; Right] tgs in
  print @@ Utils.thd3 tgs;
  [%expect {|
    { Segment_map.info = 2 -> { Segment_map.count = 0; double = `Single }, 0
      -> { Segment_map.count = 0; double = `Single }, 1
      -> { Segment_map.count = 0; double = `Single };
      stations = ((14, 15), `Lower) -> 2, ((14, 15), `Upper) -> 1,
      ((14, 5), `Lower) -> 2, ((14, 5), `Upper) -> 0, ((6, 5), `Lower) -> 0,
      ((6, 5), `Upper) -> 1 } |}];

  let tgs = build_station (6, 15) ~dirs:[Left; Right] tgs in
  print @@ Utils.thd3 tgs;
  [%expect {|
    { Segment_map.info = 2 -> { Segment_map.count = 0; double = `Single }, 3
      -> { Segment_map.count = 0; double = `Single }, 0
      -> { Segment_map.count = 0; double = `Single }, 1
      -> { Segment_map.count = 0; double = `Single };
      stations = ((14, 15), `Lower) -> 2, ((14, 15), `Upper) -> 3,
      ((6, 15), `Lower) -> 3, ((14, 5), `Lower) -> 2, ((14, 5), `Upper) -> 0,
      ((6, 5), `Lower) -> 0, ((6, 5), `Upper) -> 1, ((6, 15), `Upper) -> 1 } |}];

  let tgs = remove_station (14, 15) tgs in
  print @@ Utils.thd3 tgs;
  [%expect {|
    { Segment_map.info = 2 -> { Segment_map.count = 0; double = `Single }, 3
      -> { Segment_map.count = 0; double = `Single }, 0
      -> { Segment_map.count = 0; double = `Single }, 1
      -> { Segment_map.count = 0; double = `Single };
      stations = ((6, 15), `Lower) -> 3, ((14, 5), `Lower) -> 2,
      ((14, 5), `Upper) -> 0, ((6, 5), `Lower) -> 0, ((6, 5), `Upper) -> 1,
      ((6, 15), `Upper) -> 1 } |}];

  let tgs = remove_station (6, 15) tgs in
  print @@ Utils.thd3 tgs;
  [%expect {|
    { Segment_map.info = 2 -> { Segment_map.count = 0; double = `Single }, 0
      -> { Segment_map.count = 0; double = `Single }, 1
      -> { Segment_map.count = 0; double = `Single };
      stations = ((14, 5), `Lower) -> 2, ((14, 5), `Upper) -> 0, ((6, 5), `Lower)
      -> 0, ((6, 5), `Upper) -> 1 } |}];

  let tgs = remove_station (6, 5) tgs in
  print @@ Utils.thd3 tgs;
  [%expect {|
    { Segment_map.info = 2 -> { Segment_map.count = 0; double = `Single }, 0
      -> { Segment_map.count = 0; double = `Single };
      stations = ((14, 5), `Lower) -> 2, ((14, 5), `Upper) -> 0 } |}];

  let tgs = remove_station (14, 5) tgs in
  print @@ Utils.thd3 tgs;
  [%expect {|
    { Segment_map.info = ; stations =  } |}];
  ()



