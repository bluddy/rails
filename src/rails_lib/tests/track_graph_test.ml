module TG = Track_graph

let graph () =
  TG.make ()
  |> TG.add_ixn ~x:1 ~y:2
  |> TG.add_ixn ~x:3 ~y:4
  |> TG.add_segment ~x1:1 ~y1:2 ~dir1:Dir.Up ~x2:3 ~y2:4 ~dir2:Down ~dist:5

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
  |> TG.add_segment ~x1:1 ~y1:2 ~dir1:Dir.Up ~x2:3 ~y2:4 ~dir2:Down ~dist:5
  |> TG.add_segment ~x1:1 ~y1:2 ~dir1:Dir.UpRight ~x2:5 ~y2:6 ~dir2:DownLeft ~dist:10

let%expect_test "Graph print" =
  let g = graph () in
  TG.yojson_of_t g |> Yojson.Safe.to_string |> print_string;
  [%expect {| {"last_id":2,"graph":[[[3,4],[1,2],{"id":0,"nodes":[[1,2,["Up"]],[3,4,["Down"]]],"dist":5,"block":false}],[[5,6],[1,2],{"id":1,"nodes":[[1,2,["UpRight"]],[5,6,["DownLeft"]]],"dist":10,"block":false}]]} |}]
