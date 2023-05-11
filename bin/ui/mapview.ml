open Containers
open Mapview_d
module B = Backend

let src = Logs.Src.create "mapview" ~doc:"Mapview"
module Log = (val Logs.src_log src: Logs.LOG)

(* Mapview:
  The mapview changes fairly slowly and can thefore be mutated functionally
  Also, it should not change any game logic: it only handles viewing a window into the world
 *)

let default dims = 
  {
    cursor_x=0; cursor_y=0;
    center_x=0; center_y=0;
    zoom=Zoom4;
    dims;
    build_mode=true;
    survey=false;
    options=Options.of_list [`StationBoxes];
  }

(* let load default sexp = *)
(*   { default with *)
(*     cursor_x=int_of_ *)
(*   } *)

let get_cursor_pos v = (v.cursor_x, v.cursor_y)

let get_zoom v = v.zoom

let is_zoom4 v = match v.zoom with
  | Zoom4 -> true
  | _ -> false

let tile_size_of_zoom = function
  | Zoom1 -> 1, 1
  | Zoom2 | Zoom3 -> 8, 8
  | Zoom4 -> 16, 16

let tile_textures_of_zoom s = function
  | Zoom3 | Zoom2 -> s.State.textures.small_tiles
  | Zoom4 -> s.textures.tiles
  | Zoom1 -> failwith "tile_textures_of_zoom"

  (* For drawing code. Make sure we don't exceed map limits *)
let mapview_bounds v tile_w tile_h =
  (* Pay attention to end as well *)
  let x_delta = v.dims.w/(tile_w*2) in
  let y_delta = v.dims.h/(tile_h*2) in
  let start_x = Utils.clip (v.center_x - x_delta) ~min:0 ~max:(v.dims.w - 1 - 2 * x_delta) in
  let start_y = Utils.clip (v.center_y - y_delta + 1) ~min:0 ~max:(v.dims.h - 1 - 2 * y_delta) in
  let end_x = start_x + 2 * x_delta in
  let end_y = start_y + 2 * y_delta in
  start_x, start_y, end_x, end_y

let minimap_bounds v ~(minimap:Utils.rect) =
  let start_x = Utils.clip (v.center_x - minimap.w/2) ~min:0 ~max:(v.dims.w - minimap.w) in
  let start_y = Utils.clip (v.center_y - minimap.h/2) ~min:0 ~max:(v.dims.h - minimap.h) in
  let end_x = start_x + minimap.w in
  let end_y = start_y + minimap.h in
  start_x, start_y, end_x, end_y


let check_recenter_zoom4 v cursor_x cursor_y =
  (* recenter in zoom4 if past screen, but only in given direction *)
  let tile_w, tile_h = tile_size_of_zoom Zoom4 in
  let start_x, start_y, end_x, end_y = mapview_bounds v tile_w tile_h in
  if (cursor_y > 0 && cursor_y < start_y + 1)
      || (cursor_y < v.dims.h - 1 && cursor_y >= end_y - 1)
      || (cursor_x > 0 && cursor_x < start_x + 1)
      || (cursor_x < v.dims.w - 1 && cursor_x >= end_x - 1) then
        cursor_x, cursor_y
  else
        v.center_x, v.center_y

let move_cursor v dir n =
  let dx, dy = Dir.to_offsets dir in
  let x, y = v.cursor_x + dx * n, v.cursor_y + dy * n in
  let cursor_x = Utils.clip x ~min:0 ~max:(v.dims.w-1) in
  let cursor_y = Utils.clip y ~min:0 ~max:(v.dims.h-1) in
  let center_x, center_y = check_recenter_zoom4 v cursor_x cursor_y in
  {v with cursor_x; cursor_y; center_x; center_y}

  (* Used by menu *)
let cursor_on_woodbridge backend v =
  match B.get_track backend v.cursor_x v.cursor_y with
  | Some track when track.player = 0 ->
      begin match track.kind with
      | Bridge Wood -> true
      | _ -> false
      end
  | _ -> false

let cursor_on_station backend v =
  (* check if we're clicking on a station *)
  match B.get_track backend v.cursor_x v.cursor_y with
  | Some track when track.player = 0 ->
      begin match track.kind with
      | Station (`Depot | `Station | `Terminal) -> true
      | _ -> false
      end
  | _ -> false

let get_station_under_cursor_exn backend v =
  (* get the station the cursor is over *)
  match B.get_station backend v.cursor_x v.cursor_y with
  | Some station -> station
  | None -> failwith "No station under cursor"

let set_build_mode v mode = {v with build_mode = mode}

let get_build_mode v = v.build_mode

let get_survey v = v.survey

let set_survey v b = {v with survey=b}

let handle_event (s:State.t) (v:t) (event:Event.t) ~(minimap:Utils.rect) =

  let handle_mouse_button v x y button =
    match v.zoom with
      | Zoom1 ->
          let y = y - v.dims.y in
          {v with center_x=x; center_y=y; cursor_x=x; cursor_y=y; zoom=Zoom2}, `NoAction
      | _ when x > minimap.x && y > minimap.y && y < minimap.y + minimap.h ->
          (* click on minimap *)
          let start_x, start_y, _, _ = minimap_bounds v ~minimap in
          let x = x - minimap.x + start_x in
          let y = y - minimap.y + start_y in
          {v with center_x=x; center_y=y; cursor_x=x; cursor_y=y}, `NoAction
      | _ when x <= v.dims.x + v.dims.w && y > v.dims.y ->
          (* click in mapview *)
          let tile_w, tile_h = tile_size_of_zoom v.zoom in
          let start_x, start_y, _, _ = mapview_bounds v tile_w tile_h in
          let cursor_x = start_x + x/tile_w |> Utils.clip ~min:0 ~max:(v.dims.w - 1) in
          let cursor_y = start_y + (y-v.dims.y)/tile_h |> Utils.clip ~min:0 ~max:(v.dims.h - 1) in
          begin match v.zoom, button with
          | Zoom4, `Left when cursor_x = v.cursor_x && cursor_y = v.cursor_y ->
              (* second click *)
              if cursor_on_station s.backend v then
                v, `StationView (cursor_x, cursor_y)
              else
                let tile = B.get_tile s.backend cursor_x cursor_y in
                v, `ShowTileInfo (cursor_x, cursor_y, tile)
          | Zoom4, `Left ->
              (* move cursor *)
              let center_x, center_y = check_recenter_zoom4 v cursor_x cursor_y in
              {v with center_x; center_y; cursor_x; cursor_y}, `NoAction
          | Zoom4, `Right ->
              (* recenter *)
              {v with center_x=cursor_x; center_y=cursor_y; cursor_x; cursor_y}, `NoAction
          | (Zoom3 | Zoom2), `Left ->
              (* tile info *)
              if cursor_on_station s.backend v then
                v, `StationView (cursor_x, cursor_y)
              else
                let tile = B.get_tile s.backend cursor_x cursor_y in
                v, `ShowTileInfo (cursor_x, cursor_y, tile)
          | (Zoom3 | Zoom2), `Right ->
              {v with center_x=cursor_x; center_y=cursor_y; cursor_x; cursor_y}, `NoAction
          | _ -> v, `NoAction
          end
       | _ -> v, `NoAction
  in

  let key_to_dir = function
    | Event.Q -> Some Dir.UpLeft
    | W | Up -> Some Up
    | E -> Some UpRight
    | A | Left -> Some Left
    | D | Right -> Some Right
    | Z -> Some DownLeft
    | S | Down -> Some Down
    | C -> Some DownRight
    | _ -> None
  in

  let handle_key_zoom4 v key ~build =
    let dir = key_to_dir key in
    match dir, key with
    | None, Event.Enter ->
        begin match B.get_station s.backend v.cursor_x v.cursor_y with
        | Some station when Station.is_proper_station station ->
          v, `StationView (v.cursor_x, v.cursor_y)
        | _ ->
          let tile = B.get_tile s.backend v.cursor_x v.cursor_y in
          v, `ShowTileInfo (v.cursor_x, v.cursor_y, tile)
        end
    | None, _ -> v, `NoAction
    | Some dir, _ ->
        let move i = move_cursor v dir i in
        let msg () = Utils.{x=v.cursor_x; y=v.cursor_y; dir; player=0} in
        if build then (
          if v.build_mode then
            (* Build track *)
            match B.check_build_track s.backend ~x:v.cursor_x ~y:v.cursor_y ~dir ~player:0 with
            | `Ok -> move 1, `BuildTrack(msg ())
            | `Ferry -> move 1, `BuildFerry(msg ())
            | `HighGrade g -> v, `HighGradeTrack(msg (), g)
            | `Bridge -> v, `BuildBridge(msg ()) 
            | `Tunnel(len, g) -> v, `BuildTunnel(msg (), len, g)
            | `Illegal -> v, `NoAction
          else
            (* Remove Track *)
            match B.check_remove_track s.backend ~x:v.cursor_x ~y:v.cursor_y ~dir ~player:0 with
            | true -> move 1, `RemoveTrack(msg ())
            | false -> v, `NoAction
        ) else
          (* Regular movement *)
          move 1, `NoAction
  in

  let v, actions =
    match event with
    | Key {down=true; key=F1; _} ->
        {v with zoom = Zoom1; survey=false}, `NoAction
    | Key {down=true; key=F2; _} ->
        {v with zoom = Zoom2; survey=false}, `NoAction
    | Key {down=true; key=F3; _} ->
        {v with zoom = Zoom3; survey=false}, `NoAction
    | Key {down=true; key=F4; _} ->
        {v with zoom = Zoom4}, `NoAction
    | MouseButton {down=true; x; y; button; _} ->
        handle_mouse_button v x y button
    | Key {down=true; key; modifiers; _} when equal_zoom v.zoom Zoom4 ->
        let build = Event.Modifiers.shift modifiers in
        handle_key_zoom4 v key ~build
    | _ -> v, `NoAction
  in
  v, actions

module R = Renderer

let render win (s:State.t) (v:t) ~minimap ~build_station =
  let tile_w, tile_h = tile_size_of_zoom v.zoom in
  let tile_w2, tile_h2 = tile_w/2, tile_h/2 in
  (* In tile coordinates *)
  let start_x, start_y, end_x, end_y = mapview_bounds v tile_w tile_h in
  let iter_screen f =
    for i = 0 to v.dims.h/tile_h - 1 do
      for j = 0 to v.dims.w/tile_w - 1 do
        f i j
      done
    done
  in

  let tile_render () =
    let tiles = tile_textures_of_zoom s v.zoom in
    iter_screen (fun i j ->
      let map_x, map_y = start_x + j, start_y + i in
      let alt = ((map_x + map_y) land 1) > 0 in
      let tile = B.get_tile s.backend map_x map_y in
      let tex = Textures.TileTex.find tiles ~region:(B.get_region s.backend) ~alt tile in
      let x, y = j * tile_w, v.dims.y + i * tile_h in
      R.Texture.render win tex ~x ~y;
    )
  in

  let draw_city_names () =
    B.iter_cities (fun x y (name,_) ->
      if (x >= start_x && y >= start_y) || (x <= end_x && y <= end_y) then (
        let x = (x - start_x) * tile_w in
        let y = (y - start_y) * tile_h + v.dims.y in
        let x, y = x + 11, y - 15 in
        Fonts.Render.write win s.fonts name ~idx:4 ~x ~y ~color:Ega.black;
        let x, y = x + 1, y - 1 in
        Fonts.Render.write win s.fonts name ~idx:4 ~x ~y ~color:Ega.bcyan;
      )
    )
    s.backend
  in

  let draw_track_zoom1 () =
    B.trackmap_iter s.backend (fun x y _ ->
      R.draw_point win ~x ~y:(y + v.dims.y) ~color:Ega.black
    )
  in

  let draw_track_zoom2 () =
    iter_screen (fun i j ->
      let map_x, map_y = start_x + j, start_y + i in
      match B.get_track s.backend map_x map_y with
      | Some track ->
        let x, y = j * tile_w + tile_w2, v.dims.y + i * tile_h + tile_h2 in
        Dir.Set.iter (fun dir ->
          let dx, dy = Dir.to_offsets dir in
          R.draw_line win ~color:Ega.white ~x1:x ~y1:y ~x2:(x+dx*tile_w) ~y2:(y+dy*tile_h)
        )
        track.dirs
      | _ -> ()
    )
  in

  let draw_minimap ~(minimap:Utils.rect) =
    let from_x, from_y, from_end_x, from_end_y = minimap_bounds v ~minimap in
    R.Texture.render_subtex win s.map_tex ~x:minimap.x ~y:minimap.y
      ~from_x ~from_y ~w:minimap.w ~h:minimap.h;

    (* draw track *)
    B.trackmap_iter s.backend (fun x y _ ->
      if x >= from_x && x <= from_end_x && y >= from_y && y <= from_end_y then (
        let x = minimap.x + x - from_x in
        let y = minimap.y + y - from_y in
        R.draw_point win ~x ~y ~color:Ega.black
      )
    );

    (* minimap rectangle *)
    let x = minimap.x + start_x - from_x in
    let y = minimap.y + start_y - from_y in
    R.draw_rect win ~x ~y ~w:(end_x - start_x + 1) ~h:(end_y - start_y + 1) ~color:Ega.white
      ~fill:false;
  in

  let draw_cursor_zoom4 () =
    let x = (v.cursor_x - start_x) * tile_w in
    let y = (v.cursor_y - start_y) * tile_h + v.dims.y in
    let color = if v.build_mode then Ega.white else Ega.red in
    R.draw_rect win ~x ~y ~w:tile_w ~h:tile_h ~color ~fill:false
  in

  let draw_track_zoom4 () =
    let track_h = s.State.textures.tracks in
    iter_screen (fun i j ->
      let map_x, map_y = start_x + j, start_y + i in
      match B.get_track s.backend map_x map_y with
      | Some track ->
        let tex = Textures.Tracks.find track_h track in
        let x, y = j * tile_w, v.dims.y + i * tile_h in
        R.Texture.render win tex ~x:(x-2) ~y:(y-2)
      | _ -> ()
    )
  in

  let draw_trains_zoom4 () =
    (* For now, draw only the engine *)
    let open Textures.CarsTop in
    let offset_x, offset_y = (-Constants.tile_w/2) - 2, -2 in
    let start_x_px = start_x * Constants.tile_w in
    let start_y_px = start_y * Constants.tile_h in
    let end_x_px = end_x * Constants.tile_w in
    let end_y_px = end_y * Constants.tile_h in
    Trainmap.iter (fun (train:Train.t) ->
      (* Engine *)
      if train.x >= start_x_px - 4 && train.y >= start_y_px - 4 &&
         train.x <= end_x_px + 4 && train.y <= end_y_px + 4 then (
        let tex = Hashtbl.find s.textures.cars_top (Engine train.engine._type, train.dir) in
        let x = train.x - start_x_px + offset_x in
        let y = train.y - start_y_px + offset_y in
        R.Texture.render win tex ~x ~y
      );
      (* Cars *)
      List.iteri (fun i car ->
        let car_x, car_y, _ = Train.get_car_loc train i in
        let car_dir = Train.get_car_dir train i in
        (* if i = 0 then *)
        (*   Log.debug (fun f -> f "car_loc(%d, %d)" car_x car_y); *)
        if car_x >= start_x_px - 4 && car_y >= start_y_px - 4 &&
           car_x <= end_x_px + 4 && car_y <= end_y_px + 4 then (
          let freight = Goods.freight_of_goods car.Train.Car.good in
          let tex = Hashtbl.find s.textures.cars_top (Car freight, car_dir) in
          let x = car_x - start_x_px + offset_x in
          let y = car_y - start_y_px + offset_y in
          R.Texture.render win tex ~x ~y
        );
      ) train.cars;
    )
    s.backend.trains
  in

  let draw_survey_zoom4 () =
    iter_screen (fun i j ->
      let map_x, map_y = start_x + j, start_y + i in
      match B.get_tile s.backend map_x map_y with
      | Tile.Ocean _ -> ()
      | _ ->
        let height = (B.get_tile_height s.backend map_x map_y) / 2 |> string_of_int in
        let x, y = j * tile_w + 4, i * tile_h + 4 + v.dims.y in
        Fonts.Render.write win s.fonts height ~idx:3 ~x ~y ~color:Ega.white
    )
  in

  let draw_buildstation_mode () =
    let color = Ega.gray in
    let fill = false in
    let font = Fonts.get_font s.fonts 4 in
    let x = (v.cursor_x - start_x - 1) * tile_w in
    let font_x = x in
    let y = (v.cursor_y - start_y - 1) * tile_h + v.dims.y in
    R.draw_rect win ~x ~y ~w:(tile_w * 3) ~h:(tile_h * 3) ~color ~fill;
    Fonts.Font.write win font ~color:Ega.white "Depot" ~x:font_x ~y:(y-8);
    let x = x - tile_w in
    let y = y - tile_h in
    R.draw_rect win ~x ~y ~w:(tile_w * 5) ~h:(tile_h * 5) ~color ~fill;
    Fonts.Font.write win font ~color:Ega.white "Station" ~x:font_x ~y:(y-8);
    let x = x - tile_w in
    let y = y - tile_h in
    R.draw_rect win ~x ~y ~w:(tile_w * 7) ~h:(tile_h * 7) ~color ~fill;
    Fonts.Font.write win font ~color:Ega.white "Terminal" ~x:font_x ~y:(y-8);
  in

  R.clear_screen win;

  begin match v.zoom with
  | Zoom1 ->
      R.Texture.render win s.map_tex ~x:0 ~y:v.dims.y;
      draw_track_zoom1 ()
  | Zoom2 | Zoom3 ->
      tile_render ();
      draw_track_zoom2 ();
      draw_minimap ~minimap;
  | Zoom4 ->
      tile_render ();
      draw_city_names ();
      if v.survey && not build_station then
        draw_survey_zoom4 ();
      draw_minimap ~minimap;
      if build_station then
        draw_buildstation_mode ();
      draw_track_zoom4 ();
      draw_trains_zoom4 ();
      draw_cursor_zoom4 ();
  end;
  s

