open! Containers
module T = Track
module TM = Trackmap
module TG = Track_graph
module TS = Scan

let make_tm ?(track=Track.Track `Single) dirs = 
  T.make (Dir.Set.of_list dirs) track ~player:0

let tmap = TM.empty 20 20

let build_road ?track ?(y=10) start end_ map =
  Iter.fold
    (fun acc x -> TM.set acc ~x ~y ~t:(make_tm ?track [Left;Right]))
    map @@
    Iter.(start -- end_)

let build_road_vert ~x start end_ map =
  Iter.fold
    (fun acc y -> TM.set acc ~x ~y ~t:(make_tm [Up;Down]))
    map @@
    Iter.(start -- end_)

let square_track () =
  tmap
  |> build_road 5 15 ~y:5 
  |> build_road 5 15 ~y:15
  |> build_road_vert 5 15 ~x:5
  |> build_road_vert 5 15 ~x:15
  |> TM.set ~x:5 ~y:5 ~t:(make_tm [Down; Right])
  |> TM.set ~x:15 ~y:5 ~t:(make_tm [Down; Left])
  |> TM.set ~x:5 ~y:15 ~t:(make_tm [Up; Right])
  |> TM.set ~x:15 ~y:15 ~t:(make_tm [Up; Left])

  (* Dummy train for test purposes *)
let dummy_train tile_loc dir =
  let engine = List.hd @@ Engine.of_region Region.WestUS in
  Train.make tile_loc engine [] None ~dir ~player:0

let print_graph g = TG.yojson_of_t g |> Yojson.Safe.to_string |> print_string

