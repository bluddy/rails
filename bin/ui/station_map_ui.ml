open Containers

module R = Renderer

let make () : Edit_train_d.station_map = {
  highlighted=0;
}

let render win (s:State.t) _v =
  (* min and max over stations and ixns *)
  let min_x, min_y, max_x, max_y =
    Track_graph.G.fold_vertex (fun (x,y) (min_x, min_y, max_x, max_y) ->
      let min_x = min x min_x in
      let min_y = min y min_y in
      let max_x = max x max_x in
      let max_y = max y max_y in
      (min_x, min_y, max_x, max_y))
    s.backend.graph.graph
    (10000, 10000, 0, 0)
  in
  let dy = max_y - min_y in
  let dx = max_x - min_x in
  let adjusted_dx = dy * 4 / 3 in
  let dim = if adjusted_dx < dx then dx else adjusted_dx in
  let dim = max 10 dim in
  let map_dim = dim * 5 / 8 in
  let map_x = (min_x + max_x) / 2 - map_dim in
  let map_y = (min_y + max_y) / 2 - map_dim in

  let scale_xy x y =
    let x = (x - map_x) * 120 / dim in
    let y = (y - map_y) * 100 / dim in
    (x,y)
  in

  R.paint_screen win ~color:Ega.white;
  R.draw_rect win ~x:1 ~y:1 ~w:255 ~h:198 ~color:Ega.green ~fill:true;

  (* Draw connections *)
  Track_graph.G.iter_edges (fun (x1,y1) (x2,y2) ->
    let x1, y1 = scale_xy x1 y1 in
    let x2, y2 = scale_xy x2 y2 in
    R.draw_line win ~x1 ~y1 ~x2 ~y2 ~color:Ega.gray)
    s.backend.graph.graph;
  ()

let handle_event (_s:State.t) _v (event:Event.t) =
  if Event.pressed_esc event then
    true
  else
    false

