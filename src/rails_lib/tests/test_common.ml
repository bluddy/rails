open! Containers
module T = Track
module TM = Trackmap
module SM = Segment_map
module TG = Track_graph
module TS = Scan

let make_tm ?(track=Track.Track `Single) dirs = 
  T.make (Dir.Set.of_list dirs) track ~player:0

let tmap = TM.empty 20 20

let build_road ?(y=10) start end_ map =
  Iter.fold
    (fun acc x -> TM.set acc ~x ~y ~t:(make_tm [Left;Right]))
    map @@
    Iter.(start -- end_)

let build_road_vert ~x start end_ map =
  Iter.fold
    (fun acc y -> TM.set acc ~x ~y ~t:(make_tm [Up;Down]))
    map @@
    Iter.(start -- end_)
