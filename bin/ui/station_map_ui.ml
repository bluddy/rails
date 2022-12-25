open Containers

module R = Renderer

open Edit_train_d

let make graph train purpose : Edit_train_d.station_map =
  (* min and max over stations and ixns *)
  let min_x, min_y, max_x, max_y =
    Track_graph.G.fold_vertex (fun (x,y) (min_x, min_y, max_x, max_y) ->
      let min_x = min x min_x in
      let min_y = min y min_y in
      let max_x = max x max_x in
      let max_y = max y max_y in
      (min_x, min_y, max_x, max_y))
    graph.Track_graph.graph
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
  {
    train;
    purpose;
    map_x;
    map_y;
    map_dim;
    selected_station=None;
  }

let scale_xy v x y =
  let x = (x - v.map_x) * 120 / v.map_dim in
  let y = (y - v.map_y) * 100 / v.map_dim in
  (x,y)

let render win (s:State.t) (v:Edit_train_d.station_map) =

  R.paint_screen win ~color:Ega.white;
  R.draw_rect win ~x:1 ~y:1 ~w:255 ~h:198 ~color:Ega.green ~fill:true;

  (* Draw connections *)
  Track_graph.G.iter_edges (fun (x1,y1) (x2,y2) ->
    let x1, y1 = scale_xy v x1 y1 in
    let x2, y2 = scale_xy v x2 y2 in
    R.draw_line win ~x1 ~y1 ~x2 ~y2 ~color:Ega.gray)
    s.backend.graph.graph;

  (* Write stop text *)
  let train = Trainmap.get s.backend.trains v.train in
  let route = Train.get_route train in
  List.iter (fun (stop:Train.stop) ->
    let station = Loc_map.get_exn s.backend.stations stop.x stop.y in
    let name = Station.get_name station in
    let x, y = scale_xy v stop.x stop.y in
    Fonts.Render.write win s.fonts name ~idx:1 ~x:(x-2) ~y:(y+3) ~color:Ega.bgreen
  ) route;

  (* Draw station boxes *)
  Loc_map.iter (fun (station:Station.t) ->
    if Station.is_proper_station station then
      let color =
        match s.backend.priority with
        | Some ((x,y),_,_) when x = station.x && y = station.y -> Ega.bgreen
        | Some (_,(x,y),_) when x = station.x && y = station.y -> Ega.bgreen
        | _ -> Ega.gray
      in
      let x, y = scale_xy v station.x station.y in
      R.draw_rect win ~x:(x-1) ~y:(y-1) ~w:3 ~h:3 ~color ~fill:true
  ) s.backend.stations;

  R.draw_rect win ~x:48 ~y:1 ~w:160 ~h:9 ~color:Ega.white ~fill:true;

  (* Write heading *)
  let heading = match v.purpose with
    | `ShowRoute -> "    Route Map"
    | `EditPriority -> "Update Priority Destination"
    | `EditStop i -> Printf.sprintf "Update Scheduled Stop #%d" i
  in
  Fonts.Render.write win s.fonts ~idx:1 ~x:52 ~y:1 ~color:Ega.black heading;

  ()


  (* returns v and whether we exit *)
let handle_event (s:State.t) v (event:Event.t) =
  match event with
  | Event.MouseMotion mouse ->
    let selected_station =
      Loc_map.find (fun (station:Station.t) ->
        let x, y = scale_xy v station.x station.y in
        Station.is_proper_station station &&
          mouse.x >= x - 1 && mouse.x <= x + 1 && mouse.y >= y - 1 && mouse.y <= y + 1
      ) s.backend.stations
    in
    false, {v with selected_station}
  | Key _k when Event.pressed_esc event ->
      true, v
  | _ ->
      false, v

