open Containers
module TG = Track_graph

let graph () =
  TG.make ()
  |> TG.add_ixn ~x:1 ~y:2
  |> TG.add_ixn ~x:3 ~y:4
  |> TG.add_segment ~xyd1:(1,2,Dir.Up) ~xyd2:(3,4,Down) ~dist:5

let%expect_test "iter_succ_ixn_dirs" =
  let graph = graph () in
  TG.iter_succ_ixn_dirs (fun _ixn dir ->
    print_string @@ Dir.show dir
  ) graph ~ixn:(1,2);
  [%expect {| Dir.Down |}]

let graph () =
  TG.make ()
  |> TG.add_ixn ~x:1 ~y:2
  |> TG.add_ixn ~x:3 ~y:4
  |> TG.add_ixn ~x:5 ~y:6
  |> TG.add_ixn ~x:7 ~y:8
  |> TG.add_segment ~xyd1:(1,2,Dir.Up) ~xyd2:(3,4,Right) ~dist:5
  |> TG.add_segment ~xyd1:(1,2,Dir.UpRight) ~xyd2:(5,6,Left) ~dist:10
  |> TG.add_segment ~xyd1:(5,6,Dir.UpRight) ~xyd2:(7,8,DownLeft) ~dist:3

let print_graph g =
  TG.yojson_of_t g |> Yojson.Safe.to_string |> print_string

let%expect_test "graph print" =
  let g = graph () in
  print_graph g;
  [%expect {| {"last_id":3,"graph":[[[3,4],[1,2],{"id":0,"nodes":[[1,2,["Up"]],[3,4,["Right"]]],"dist":5,"block":false}],[[5,6],[1,2],{"id":1,"nodes":[[1,2,["UpRight"]],[5,6,["Left"]]],"dist":10,"block":false}],[[7,8],[5,6],{"id":2,"nodes":[[5,6,["UpRight"]],[7,8,["DownLeft"]]],"dist":3,"block":false}]]} |}]

let%expect_test "graph remove segment" =
  let g = graph ()
    |> TG.remove_segment ~xyd:(1,2,Dir.Up)
  in
  print_graph g;
  [%expect {| {"last_id":3,"graph":[[[5,6],[1,2],{"id":1,"nodes":[[1,2,["UpRight"]],[5,6,["Left"]]],"dist":10,"block":false}],[[7,8],[5,6],{"id":2,"nodes":[[5,6,["UpRight"]],[7,8,["DownLeft"]]],"dist":3,"block":false}]]} |}]

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
  let map = Loc_map.empty
    |> Loc_map.add (1,2) 10
    |> Loc_map.add (3,4) 11
    |> Loc_map.add (5,6) 12
  in
  let g = graph () in
  let res = TG.connected_stations_dirs g map (1,2) |> Iter.to_list in
  List.pp (Pair.pp (Pair.pp Int.pp Int.pp) Dir.pp) Format.std_formatter res;
  [%expect {|
    3, 4, Dir.Right, 5, 6,
    Dir.Left |}]

module Track = struct
  module TM = Trackmap

  let track dirs = 
    Track.make (Dir.Set.of_list dirs) (Track `Single) ~player:0

  let station dirs = 
    Track.make (Dir.Set.of_list dirs) (Station `Station) ~player:0

  let dirs = [Dir.Left; Right]
  let std_map =
    TM.empty 7 7
    |> TM.set ~x:1 ~y:1 ~t:(track @@ UpLeft::dirs)
    |> TM.set ~x:2 ~y:1 ~t:(track dirs)
    |> TM.set ~x:3 ~y:1 ~t:(track dirs)
    |> TM.set ~x:4 ~y:1 ~t:(track dirs)
    |> TM.set ~x:5 ~y:1 ~t:(track @@ UpRight::dirs)

  let%expect_test "build_station half track" =
    (* x--- map *)
    let map = std_map
      |> TM.remove ~x:5 ~y:1
    in
    let scan1 = TM.Search.scan map ~x:5 ~y:1 ~player:0 in
    (* Add station x x---s *)
    let map = TM.set map ~x:5 ~y:1 ~t:(station dirs) in
    let scan2 = TM.Search.scan map ~x:5 ~y:1 ~player:0 in
    (* Corresponding graph *)
    let g = TG.make ()
      |> TG.add_ixn ~x:1 ~y:1
    in
    print_graph g;
    [%expect {| {"last_id":0,"graph":[]} |}];
    let g = TG.Track.handle_build_station g ~x:5 ~y:1 scan1 scan2 in
    print_graph g;
    [%expect {| {"last_id":0,"graph":[]} |}]

  let%expect_test "build_station mid track" =
    (* x---x map *)
    let map = std_map in
    let scan1 = TM.Search.scan map ~x:3 ~y:1 ~player:0 in
    (* Add station in middle x-s-x *)
    let map = TM.set map ~x:3 ~y:1 ~t:(station dirs) in
    let scan2 = TM.Search.scan map ~x:3 ~y:1 ~player:0 in
    (* Corresponding graph *)
    let g = TG.make ()
      |> TG.add_ixn ~x:1 ~y:1
      |> TG.add_ixn ~x:4 ~y:1
      |> TG.add_segment ~xyd1:(1,1,Right) ~xyd2:(5,1,Left) ~dist:5
    in
    print_graph g;
    [%expect {| {"last_id":1,"graph":[[[5,1],[1,1],{"id":0,"nodes":[[1,1,["Right"]],[5,1,["Left"]]],"dist":5,"block":false}]]} |}];
    let g = TG.Track.handle_build_station g ~x:3 ~y:1 scan1 scan2 in
    print_graph g;
    [%expect {| {"last_id":3,"graph":[[[3,1],[1,1],{"id":1,"nodes":[[1,1,["Right"]],[3,1,["Left"]]],"dist":2,"block":false}],[[5,1],[3,1],{"id":2,"nodes":[[5,1,["Left"]],[3,1,["Right"]]],"dist":2,"block":false}]]} |}]

  let%expect_test "build_track" = ()
  let%expect_test "build_track_complex" = ()
  let%expect_test "remove_track" = ()

end
