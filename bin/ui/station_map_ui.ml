open Containers

module R = Renderer

open Edit_train_d

let selection_dist = 24
let blink_time = 500

let make graph train state : Edit_train_d.station_map =
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
    state;
    map_x;
    map_y;
    map_dim;
    selected_station=None;
    flash_time=0;
    flash_on=false;
  }

  (* Function used to scale to the screen *)
let scale_xy v x y =
  let x = (x - v.map_x) * 120 / v.map_dim in
  let y = (y - v.map_y) * 100 / v.map_dim in
  (x,y)

let render win (s:State.t) (v:Edit_train_d.station_map) =
  let write ?(color=Ega.black) ?(active_color=Ega.white) ?(idx=1) =
    Fonts.Render.write win s.fonts ~idx ~color ~active_color
  in

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
  List.iteri (fun i (stop:Train.stop) ->
    let station = Loc_map.get_exn s.backend.stations stop.x stop.y in
    let name = Printf.sprintf "%d.%s" (i+1) (Station.get_name station) in
    let x, y = scale_xy v stop.x stop.y in
    write name ~x:(x-2) ~y:(y+3) ~color:Ega.bgreen
  ) route;

  (* Priority stop *)
  begin match train.priority with
  | Some stop -> 
    let station = Loc_map.get_exn s.backend.stations stop.x stop.y in
    let name = Printf.sprintf "P:%s" (Station.get_name station) in
    let x, y = scale_xy v stop.x stop.y in
    write name ~x:(x-2) ~y:(y+3) ~color:Ega.bgreen
  | None -> ()
  end;

  (* Draw station boxes *)
  Loc_map.iter (fun (station:Station.t) ->
    if Station.is_proper_station station then
      let color =
        match v.selected_station, s.backend.priority with
        | Some (x,y), _ when x = station.x && y = station.y && v.flash_on -> Ega.white
        | Some (x,y), _ when x = station.x && y = station.y -> Ega.black
        | _, Some ((x,y),_,_) when x = station.x && y = station.y -> Ega.bgreen
        | _, Some (_,(x,y),_) when x = station.x && y = station.y -> Ega.bgreen
        | _ -> Ega.gray
      in
      let x, y = scale_xy v station.x station.y in
      R.draw_rect win ~x:(x-1) ~y:(y-1) ~w:3 ~h:3 ~color ~fill:true
  ) s.backend.stations;

  R.draw_rect win ~x:48 ~y:1 ~w:160 ~h:9 ~color:Ega.white ~fill:true;

  (* Write heading *)
  let heading = match v.state with
    | `ShowRoute -> "    Route Map"
    | `EditPriority -> "Update Priority Destination"
    | `EditStop i -> Printf.sprintf "Update Scheduled Stop #%d" i
  in
  write ~x:52 ~y:1 heading;

  (* Info bar *)
  begin match v.selected_station with
  | Some (x,y) ->
      let write = write ~idx:4 in
      let station = Loc_map.get_exn s.backend.stations x y in
      let demand = Station.get_demand_exn station in
      write ~x:258 ~y:1 @@ Station.get_name station;
      write ~x:258 ~y:13 @@ Printf.sprintf "(%s)" @@ Station.kind_str station;

      let supply = Station.get_supply_exn station in
      write ~x:258 ~y:25 "Waiting";
      let y =
        List.fold_left (fun y good ->
          match Hashtbl.find_opt supply good with
          | Some amount ->
              let cars = amount / Goods.full_car in
              if cars > 0 then (
                let tex = Hashtbl.find s.textures.route_cars @@ `CarOld good in
                R.Texture.render win ~x:258 ~y tex;
                write ~x:292 ~y:(y+1) @@ Printf.sprintf "(%d)" cars;
                y + 10)
              else
                y
          | _ -> y)
        34
        Goods.order
      in
      let y = write ~x:258 ~y "Demands"; y + 8 in
      ignore @@
        Goods.Set.fold (fun good y ->
          write ~x:258 ~y @@ Goods.show good;
          y + 8)
        demand
      y

  | _ -> ()
  end;

  (* Remove Station box *)
  begin match v.state with
    | `EditPriority | `EditStop _ ->
        R.draw_rect win ~x:256 ~y:182 ~w:64 ~h:18 ~color:Ega.cyan ~fill:true;
        R.draw_rect win ~x:256 ~y:182 ~w:64 ~h:18 ~color:Ega.black ~fill:false;
        write ~x:272 ~y:184 "&Remove\nStation"
    | _ -> ()
  end;
  ()

let nobaction = Backend.Action.NoAction

  (* returns v and whether we exit *)
let handle_event (s:State.t) v (event:Event.t) =
  let remove_stop () =
    let stop = match v.state with
      | `EditPriority -> `Priority
      | `EditStop i -> `Stop i
      | _ -> assert false
    in
    let b_action = Backend.Action.RemoveStop {train=v.train; stop} in
    false, v, b_action
  in
  match event, v.selected_station, v.state with
  | Event.MouseMotion mouse, _, _ ->
    let selected_station =
      Loc_map.fold (fun (station:Station.t) closest ->
        let x, y = scale_xy v station.x station.y in
        let dist = Utils.classic_dist (x,y) (mouse.x,mouse.y) in
        match closest with
        | None when Station.is_proper_station station && dist < selection_dist ->
            Some ((station.x, station.y), dist)
        | Some (_, min_dist) when Station.is_proper_station station && dist < selection_dist && dist < min_dist ->
            Some ((station.x, station.y), dist)
        | _ -> closest
      )
      s.backend.stations
      ~init:None
    in
    let selected_station =
      match selected_station with
      | Some (loc, _ ) -> Some loc
      | None -> None
    in
    false, {v with selected_station}, nobaction

    (* Remove stop box *)
  | Event.MouseButton {button=`Left; down=true; x; y; _}, _, (`EditPriority | `EditStop _) when x >= 256 && y >= 182 ->
      remove_stop ()

  | Event.Key {key=Event.R; down=true; _}, _, (`EditPriority | `EditStop _) ->
      remove_stop ()

  | Event.MouseButton {button=`Left; down=true; _}, Some station, `EditPriority ->
      let b_action =
        Backend.Action.SetStopStation {train=v.train; stop=`Priority; station}
      in
      false, v, b_action

  | Event.MouseButton {button=`Left; down=true; _}, Some station, `EditStop stop  ->
      let b_action =
        if Backend.check_stop_station s.backend ~train:v.train ~stop ~station then
          Backend.Action.SetStopStation {train=v.train; stop=`Stop stop; station}
        else nobaction
      in
      false, v, b_action

  | Key _k, _, _ when Event.pressed_esc event ->
      true, v, nobaction
  | _ ->
      false, v, nobaction

let handle_tick v time =
  if time - v.flash_time > blink_time then (
    v.flash_time <- time;
    v.flash_on <- not v.flash_on;
  );
  ()


