open Containers
open Mapview_d
module Hashtbl = Utils.Hashtbl
module B = Backend
module C = Constants

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
    smoke_plumes=[];
    tile_buffer=Tilebuffer.create 70 50; (* TODO: remove hardcoding *)
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
  | Zoom2 -> 4, 4
  | Zoom3 -> 8, 8
  | Zoom4 -> 16, 16

  (* How much we need to divide the normal screen coordinates by *)
let tile_div_of_zoom = function
  | Zoom1 -> 16
  | Zoom2 -> 4
  | Zoom3 -> 2
  | Zoom4 -> 1

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
  let start_x_tile = Utils.clip (v.center_x - minimap.w/2) ~min:0 ~max:(v.dims.w - minimap.w) in
  let start_y_tile = Utils.clip (v.center_y - minimap.h/2) ~min:0 ~max:(v.dims.h - minimap.h) in
  let end_x_tile = start_x_tile + minimap.w in
  let end_y_tile = start_y_tile + minimap.h in
  start_x_tile, start_y_tile, end_x_tile, end_y_tile

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
  match B.get_station (v.cursor_x, v.cursor_y) backend with
  | Some station -> station
  | None -> failwith "No station under cursor"

let set_build_mode v mode = {v with build_mode = mode}

let get_build_mode v = v.build_mode

let get_survey v = v.survey

let set_survey v b = {v with survey=b}

let update_option v option value =
  let options = 
    if value then
      Options.add v.options option
    else
      Options.remove v.options option
  in
  {v with options}


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
    match key_to_dir key with
    | Some dir ->
        let move i = move_cursor v dir i in
        let msg () = Utils.{x=v.cursor_x; y=v.cursor_y; dir; player=0} in
        if build then (
          if v.build_mode then
            (* Build track *)
            match B.check_build_track s.backend ~x:v.cursor_x ~y:v.cursor_y ~dir ~player:0 with
            | `Ok -> move 1, `BuildTrack(msg ())
            | `Ferry -> move 1, `BuildFerry(msg ())
            | `HighGrade g -> v, `HighGradeTrack(msg (), g, false)
            | `Bridge -> v, `BuildBridge(msg ()) 
            | `Tunnel g -> v, `HighGradeTrack(msg (), g, true)
            | `Illegal -> v, `NoAction
          else
            (* Remove Track *)
            match B.check_remove_track s.backend ~x:v.cursor_x ~y:v.cursor_y ~dir ~player:0 with
            | true -> move 1, `RemoveTrack(msg ())
            | false -> v, `NoAction
        ) else
          (* Regular movement *)
          move 1, `NoAction
    | None ->
      match key with
      | Event.Enter ->
          begin match B.get_station (v.cursor_x, v.cursor_y) s.backend with
          | Some station when Station.is_proper_station station ->
            v, `StationView (v.cursor_x, v.cursor_y)
          | _ ->
            let tile = B.get_tile s.backend v.cursor_x v.cursor_y in
            v, `ShowTileInfo (v.cursor_x, v.cursor_y, tile)
          end
      | Event.K1 when build &&
            B.check_make_single_track s.backend ~x:v.cursor_x ~y:v.cursor_y ->
              v, `DoubleTrack(false, v.cursor_x, v.cursor_y)
      | Event.K2 when build &&
            B.check_make_double_track s.backend ~x:v.cursor_x ~y:v.cursor_y ->
              v, `DoubleTrack(true, v.cursor_x, v.cursor_y)
      | _ -> v, `NoAction
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
  let tile_div = tile_div_of_zoom v.zoom in
  let tile_w2, tile_h2 = tile_w/2, tile_h/2 in
  (* In tile coordinates: where our view starts *)
  let start_x, start_y, end_x, end_y = mapview_bounds v tile_w tile_h in
  let start_x_map, start_y_map = start_x * C.tile_w, start_y * C.tile_h in
  let end_x_map, end_y_map = end_x * C.tile_w, end_y * C.tile_h in
  let iter_screen f =
    for y = 0 to v.dims.h/tile_h - 1 do
      for x = 0 to v.dims.w/tile_w - 1 do
        f x y
      done
    done
  in
  let is_in_view x y =
    (* In map coordinates *)
    x >= start_x_map - C.draw_margin && y >= start_y_map - C.draw_margin &&
    x <= end_x_map + C.draw_margin && y <= end_y_map + C.draw_margin
  in
  let tile_render () =
    let tiles = tile_textures_of_zoom s v.zoom in
    iter_screen (fun x y ->
      let tile_x, tile_y = start_x + x, start_y + y in
      (* Check for alternate tile *)
      let alt = ((tile_x + tile_y) land 1) > 0 in
      let tile = B.get_tile s.backend tile_x tile_y in
      let tex = Textures.TileTex.find tiles ~region:(B.get_region s.backend) ~alt tile in
      let x, y = v.dims.x + x * tile_w, v.dims.y + y * tile_h in
      R.Texture.render win tex ~x ~y;
    )
  in
  let draw_city_names () =
    B.iter_cities (fun tile_x tile_y (name,_) ->
      if tile_x >= start_x && tile_y >= start_y && tile_x <= end_x && tile_y <= end_y then (
        let x = (tile_x - start_x) * tile_w in
        let y = (tile_y - start_y) * tile_h + v.dims.y in
        let x, y = x + 11, y - 15 in
        Fonts.Render.write win s.fonts name ~idx:4 ~x ~y ~color:Ega.black;
        let x, y = x + 1, y - 1 in
        Fonts.Render.write win s.fonts name ~idx:4 ~x ~y ~color:Ega.bcyan;
      )
    )
    s.backend
  in
  let draw_track_and_trains_zoom1 from_x from_y from_x_end from_y_end start_x start_y =
    B.trackmap_iter s.backend (fun x y _ ->
      if x >= from_x && x <= from_x_end && y >= from_y && y <= from_y_end then (
        let x, y = start_x + x - from_x, start_y + y - from_y in
        R.draw_point win ~x ~y ~color:Ega.black
      )
    );
    (* Draw train *)
    Trainmap.iter (fun (train:Train.t) ->
      (* NOTE: Is the 2nd black point necessary? *)
      List.iter (fun (pixels, color) ->
        let x_map, y_map, _ = Train.calc_car_loc_in_pixels train s.backend.track pixels in
        (* Must use tile_dim: tile_div changes by zoom *)
        let x_tile, y_tile = x_map / C.tile_dim, y_map / C.tile_dim in
        if x_tile >= from_x && x_tile <= from_x_end && y_tile >= from_y && y_tile <= from_y_end then (
          let x, y = start_x + x_tile - from_x, start_y + y_tile - from_y in
          R.draw_point win ~color ~x ~y
        ))
      [0, Ega.white; 16, Ega.black]
    ) s.backend.trains
  in
  let draw_track_zoom2_3 mult =
    iter_screen @@ fun x y ->
      let tile_x, tile_y = start_x + x, start_y + y in
      match B.get_track s.backend tile_x tile_y with
      | Some track ->
        let x = v.dims.x + x * tile_w + tile_w2 in
        let y = v.dims.y + y * tile_h + tile_h2 in

        let draw_signals () =
          let station = Loc_map.get_exn (tile_x, tile_y) s.backend.stations in
          let mult = mult + 1 in
          Dir.Set.iter (fun dir ->
            (* Only draw signal if we have track in this direction *)
            let tile_x, tile_y = Dir.adjust dir tile_x tile_y in
            match B.get_track s.backend tile_x tile_y with
            | None -> ()
            | Some _ -> 
              let signal = Station.get_signal station dir in
              let dir90 = dir |> Dir.cw |> Dir.cw in
              let dx, dy = Dir.to_offsets dir90 in
              let dx, dy = mult * dx, mult * dy in
              let x, y = x + dx, y + dy in
              (* draw frame *)
              let color = Station.frame_color_of_signal signal in
              R.draw_rect win ~color ~x:(x-2) ~y:(y-2) ~w:4 ~h:4 ~fill:false;
              (* draw light *)
              let color = Station.color_of_signal signal in
              R.draw_rect win ~color ~x:(x-1) ~y:(y-1) ~w:2 ~h:2 ~fill:true
          )
          track.dirs
        in
        begin match track.kind with
        | Station `Depot
        | Station `Station
        | Station `Terminal ->
            (* Draw station outline *)
            R.draw_rect win ~color:Ega.white ~x:(x-3) ~y:(y-3) ~w:6 ~h:6 ~fill:true;
            draw_signals ()

        | Station `SignalTower -> draw_signals ()

        | _normal_track ->
            let draw_segment x y dir =
              let dx, dy = Dir.to_offsets dir in
              R.draw_line win ~color:Ega.white ~x1:x ~y1:y
                ~x2:(x+dx*tile_w2) ~y2:(y+dy*tile_h2);
            in
            let is_double = Track.acts_like_double track in
            Dir.Set.iter (fun dir ->
              draw_segment x y dir;
              if is_double then (
                let dir_adjust = match dir with
                | Dir.Up | Down -> Dir.Left
                | _ -> Down
                in
                let x, y = Dir.adjust dir_adjust x y in
                draw_segment x y dir
              )
            )
            track.dirs
        end
      | _ -> ()
  in
  let draw_trains_zoom2_3 () =
    let offset_x, offset_y = v.dims.x - 1, v.dims.y - 1 in
    let draw_car_or_engine color x y dir =
      if is_in_view x y then (
        let x = (x - start_x_map)/tile_div + offset_x in
        let y = (y - start_y_map)/tile_div + offset_y in
        R.draw_rect win ~x ~y ~w:2 ~h:2 ~color ~fill:true;
        match v.zoom with
        | Zoom3 ->
          let dir = Dir.opposite dir in
          let x, y = Dir.adjust dir x y in
          R.draw_rect win ~x ~y ~w:2 ~h:2 ~color ~fill:true;
        | _ -> ()
      )
    in
    Trainmap.iter (fun (train:Train.t) ->
      (* Draw cars *)
      List.iteri (fun i car ->
        let x, y, dir = Train.calc_car_loc_in_pixels train s.backend.track @@ (i+1)*8 in
        let color = Train.Car.get_freight car |> Goods.color_of_freight ~full:true in
        let dir = Dir.opposite dir in
        draw_car_or_engine color x y dir
      ) train.cars;
      (* Draw engine *)
      let x, y, dir = Train.calc_car_loc_in_pixels train s.backend.track 0 in
      draw_car_or_engine Ega.black x y dir;
    ) s.backend.trains;
  in
  let draw_stationboxes mult size =
    (* mult and size are in tiles! *)
    Tilebuffer.clear v.tile_buffer;
    let copy_to_tile_buffer () =
      iter_screen @@ fun x y ->
        let tile_x, tile_y = start_x + x, start_y + y in
        if Option.is_some @@ B.get_track s.backend tile_x tile_y then
          Tilebuffer.set v.tile_buffer x y
    in copy_to_tile_buffer ();
    (* We need to find an empty screen location to draw the station boxes *)
    let find_space_for_stationbox tile_x tile_y =
      (* tile_x/y: visible tiles on screen, not from origin *)
      let num_tiles_x, num_tiles_y = v.dims.w / tile_w, v.dims.h / tile_h in
      let rec search_for_box_space i =
        let x_offset, y_offset = Dir.to_offsets_int i in
        let x_offset, y_offset = x_offset * mult - (size/2), y_offset * mult - (size/2) in
        let tile_x, tile_y = tile_x + x_offset, tile_y + y_offset in
        let tile_x = Utils.clip ~min:0 ~max:(num_tiles_x - size - 1) tile_x in
        let tile_y = Utils.clip ~min:0 ~max:(num_tiles_y - size - 1) tile_y in
        if Tilebuffer.is_empty_box v.tile_buffer tile_x tile_y ~w:size ~h:size || i >= 24 then
          tile_x, tile_y
        else
          search_for_box_space (i+1)
      in
      search_for_box_space 0
    in
    let draw_stationbox revenue s_tile_x s_tile_y tile_x tile_y =
      (* tile_x/y: in terms of on-screen tiles *)
      let station_x, station_y = s_tile_x * tile_w + v.dims.x + tile_w2, s_tile_y * tile_h + v.dims.y + tile_w2 in
      (* Mark box in buffer *)
      let box_x, box_y = tile_x * tile_w + v.dims.x, tile_y * tile_w + v.dims.y in
      let w, h = size * tile_w, size * tile_h in
      R.draw_line win ~x1:(box_x+16) ~y1:box_y ~x2:station_x ~y2:station_y ~color:Ega.white;
      R.draw_rect win ~x:box_x ~y:box_y ~w ~h ~fill:true ~color:Ega.bblue;
      R.draw_rect win ~x:box_x ~y:box_y ~w ~h ~fill:false ~color:Ega.white; (* frame *)
    in
    iter_screen @@ fun x y ->
      let (tile_x, tile_y) as loc = start_x + x, start_y + y in
      if tile_x >= start_x && tile_x < end_x - size &&
         tile_y >= start_y && tile_y < end_y - size then (
        B.get_track s.backend tile_x tile_y
        |> Option.iter (fun track ->
          if Track.is_big_station track then
            let station = Station_map.get_exn loc s.backend.stations in
            let revenue = Station.total_goods_revenue station in
            let box_x, box_y = find_space_for_stationbox x y in
            Tilebuffer.set_box v.tile_buffer box_x box_y ~w:size ~h:size;
            draw_stationbox revenue x y box_x box_y
      ))
  in
  let draw_minimap ~(minimap:Utils.rect) =
    let from_x, from_y, from_x_end, from_y_end = minimap_bounds v ~minimap in
    R.Texture.render_subtex win s.map_tex ~x:minimap.x ~y:minimap.y
      ~from_x ~from_y ~w:minimap.w ~h:minimap.h;
    draw_track_and_trains_zoom1 from_x from_y from_x_end from_y_end minimap.x minimap.y;
    (* Draw white frame showing visible area. Tile = pixel *)
    let x = start_x - from_x + minimap.x in
    let y = start_y - from_y + minimap.y in
    let w, h = end_x - start_x, end_y - start_y in
    R.draw_rect win ~x ~y ~w ~h ~color:Ega.white ~fill:false
  in
  let draw_cursor_zoom4 () =
    let x = (v.cursor_x - start_x) * tile_w in
    let y = (v.cursor_y - start_y) * tile_h + v.dims.y in
    let color = if v.build_mode then Ega.white else Ega.red in
    R.draw_rect win ~x ~y ~w:tile_w ~h:tile_h ~color ~fill:false
  in
  let draw_track_zoom4 () =
    let track_h = s.State.textures.tracks in
    iter_screen (fun x y ->
      let map_x, map_y = start_x + x, start_y + y in
      let x, y = v.dims.x + x * tile_w, v.dims.y + y * tile_h in
      match B.get_track s.backend map_x map_y with
      | Some track when Track.is_double track ->
        let tex = Textures.Tracks.find track_h track in
        let (x1, y1), (x2, y2) = Track.double_track_offsets track in
        let xd, yd = x + x1 - 2, y + y1 - 2 in
        R.Texture.render win tex ~x:xd ~y:yd;
        let xd, yd = x + x2 - 2, y + y2 - 2 in
        R.Texture.render win tex ~x:xd ~y:yd

      | Some track ->
        let tex = Textures.Tracks.find track_h track in
        R.Texture.render win tex ~x:(x-2) ~y:(y-2)

      | _ -> ()
    )
  in
  let draw_trains_zoom4 () =
    let open Textures.CarsTop in
    let offset_x, offset_y = (-C.tile_w/2) - 2, v.dims.y -C.tile_h/2 - 2 in
    Trainmap.iter (fun (train:Train.t) ->
      (* Draw cars *)
      List.iteri (fun i car ->
        let car_x, car_y, car_dir =
          Train.calc_car_loc ~car_pixels:12 train s.backend.track i
        in
        if is_in_view car_x car_y then (
          let freight = Goods.freight_of_goods car.Train.Car.good in
          let tex = Hashtbl.find s.textures.cars_top (Car freight, car_dir) in
          let x, y = car_x - start_x_map + offset_x, car_y - start_y_map + offset_y in
          R.Texture.render win tex ~x ~y
        );
      ) train.cars;
      (* Draw engine *)
      if is_in_view train.x train.y then (
        let tex = Hashtbl.find s.textures.cars_top
          (Engine train.engine._type, train.dir)
        in
        let x, y = 
          Train.adjust_loc_for_double_track s.backend.track train.x train.y train.dir
        in
        let x, y = x - start_x_map + offset_x, y - start_y_map + offset_y in
        R.Texture.render win tex ~x ~y
      )
    )
    s.backend.trains;
    (* Draw smoke *)
    let smoke_texs = Hashtbl.find s.textures.Textures.smoke `SmokeTop in
    let offset_x, offset_y = (-C.tile_w/2) - 2, -2 in
    List.iter (fun smoke ->
      if is_in_view smoke.x smoke.y then (
        let x, y = smoke.x - start_x_map + offset_x, smoke.y - start_y_map + offset_y in
        let tex = smoke_texs.(smoke.frame/4) in
        R.Texture.render win tex ~x ~y
      ))
    v.smoke_plumes
  in
  let draw_survey_zoom4 () =
    iter_screen (fun x y ->
      let map_x, map_y = start_x + x, start_y + y in
      match B.get_tile s.backend map_x map_y with
      | Tile.Ocean _ -> ()
      | _ ->
        let height = (B.get_tile_height s.backend map_x map_y) / 2 |> string_of_int in
        let x, y = x * tile_w + 4 + v.dims.x, y * tile_h + 4 + v.dims.y in
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
      draw_track_and_trains_zoom1 0 0 v.dims.w v.dims.h v.dims.x v.dims.y
  | Zoom2 ->
      draw_track_zoom2_3 1;
      draw_trains_zoom2_3 ();
      draw_minimap ~minimap;
      if Options.mem v.options `StationBoxes then (
        draw_stationboxes 6 8
      )
  | Zoom3 ->
      tile_render ();
      draw_track_zoom2_3 2;
      draw_trains_zoom2_3 ();
      draw_minimap ~minimap;
      if Options.mem v.options `StationBoxes then (
        draw_stationboxes 3 4
      )
  | Zoom4 ->
      tile_render ();
      draw_city_names ();
      if build_station then (
        draw_buildstation_mode ())
      else if v.survey then (
        draw_survey_zoom4 ()
      );
      draw_track_zoom4 ();
      draw_trains_zoom4 ();
      draw_minimap ~minimap;
      draw_cursor_zoom4 ();
  end;
  s

let handle_tick (s:State.t) (v:t) _time is_cycle =
  if not is_cycle then v
  else
  (* Move smoke *)
  let smoke_plumes =
    if s.backend.cycle mod 3 = 0 then (
      List.iter (fun plume ->
        let x, y = Dir.adjust plume.dir plume.x plume.y in
        plume.x <- x;
        plume.y <- y;
        plume.frame <- plume.frame + 1;
      )
      v.smoke_plumes;
      List.filter (fun plume ->
        plume.frame < max_smoke_frame)
      v.smoke_plumes
    ) else
      v.smoke_plumes
  in
  let smoke_plumes =
    match v.zoom with
    | Zoom4 ->
      (* Only create smoke plumes for the drawn area *)
      let tile_w, tile_h = tile_size_of_zoom v.zoom in
      let start_x, start_y, end_x, end_y = mapview_bounds v tile_w tile_h in
      let start_x_map = start_x * C.tile_w in
      let start_y_map = start_y * C.tile_h in
      let end_x_map = end_x * C.tile_w in
      let end_y_map = end_y * C.tile_h in
      (* Create plumes of smoke *)
      let smoke_plumes =
        Trainmap.foldi (fun i acc (train:Train.t) ->
          if Engine.has_steam train.engine &&
             Train.get_speed train > 0 &&
             (i * 3 + s.backend.cycle) mod 16 = 0 &&
             train.x >= start_x_map - C.draw_margin &&
             train.x <= end_x_map + C.draw_margin &&
             train.y >= start_y_map - C.draw_margin &&
             train.y <= end_y_map + C.draw_margin then
            let smoke =
              {frame=0; x=train.x; y=train.y; dir=Dir.cw train.dir}
            in
            smoke::acc
          else
            acc
        )
        ~init:smoke_plumes
        s.backend.trains
      in
      smoke_plumes
    | _ -> smoke_plumes
  in
  [%upf v.smoke_plumes <- smoke_plumes];
  v



