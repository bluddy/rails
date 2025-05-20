open! Containers
module T = Track
module TM = Trackmap
module TG = Track_graph
module TS = Scan
module C = Constants

let make_tm ?(track=Track.Track `Single) dirs = 
  T.make (Dir.Set.of_list dirs) track C.player

let tmap = TM.empty 20 20

let build_road ?track ?(y=10) start end_ map =
  Iter.fold
    (fun acc x -> TM.set_xy x y (make_tm ?track [Left;Right]) acc)
    map @@
    Iter.(start -- end_)

let build_road_vert ~x start end_ map =
  Iter.fold
    (fun acc y -> TM.set_xy x y (make_tm [Up;Down]) acc)
    map @@
    Iter.(start -- end_)

let square_track () =
  tmap
  |> build_road 5 15 ~y:5 
  |> build_road 5 15 ~y:15
  |> build_road_vert 5 15 ~x:5
  |> build_road_vert 5 15 ~x:15
  |> TM.set (5, 5) (make_tm [Down; Right])
  |> TM.set (15, 5) (make_tm [Down; Left])
  |> TM.set (5, 15) (make_tm [Up; Right])
  |> TM.set (15, 15) (make_tm [Up; Left])

  (* Dummy train for test purposes *)
let dummy_train tile_loc dir =
  let engine = List.hd @@ Engine.of_region Region.WestUS in
  Train.make tile_loc engine [] None ~dir ~player:C.player

let print_graph g = TG.yojson_of_t g |> Yojson.Safe.to_string |> print_string

