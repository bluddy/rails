module TG = Track_graph


let%expect_test "Graph succ test" =
  let graph =
    TG.make ()
    |> TG.add_ixn ~x:1 ~y:2
    |> TG.add_ixn ~x:3 ~y:4
    |> TG.add_segment ~x1:1 ~y1:2 ~dir1:Dir.Up ~x2:3 ~y2:4 ~dir2:Down ~dist:5
  in
  TG.iter_succ_ixn_dirs (fun ixn dir ->
    print_string @@ Dir.show dir
  ) graph ~ixn:(1,2);
  [%expect {| Dir.Down |}]

