open Containers
open Mapview_d
module Hashtbl = Utils.Hashtbl
module B = Backend
module C = Constants
module R = Renderer
module M = Money
open! Utils.Infix

let src = Logs.Src.create "mapview" ~doc:"Mapview"
module Log = (val Logs.src_log src: Logs.LOG)

(* Mapview:
  The mapview changes fairly slowly and can thefore be mutated functionally
  Also, it should not change any game logic: it only handles viewing a window into the world
 *)

let default dims = 
  {
    const_box_x = 0; const_box_y = 0;
    center_x = 0; center_y = 0;
    zoom = Zoom4;
    dims;
    build_mode = true;
    survey = false;
    smoke_plumes = [];
    draw_buffer = Hashtbl.create 10;
    tile_buffer = Tilebuffer.create 70 50; (* TODO: remove hardcoding *)
    options = Options.of_list [`StationBoxes];
  }

let get_cursor_pos v = (v.const_box_x, v.const_box_y)

let get_zoom v = v.zoom

let is_zoom4 v = match v.zoom with
  | Zoom4 -> true
  | _ -> false

(* Keep the same zoom *)
let with_zoom_23 v f =
  let zoom = match v.zoom with
    | Zoom2 x -> Zoom2 (f x)
    | Zoom3 x -> Zoom3 (f x)
    | x -> x
  in
  {v with zoom}

let tile_size_of_zoom = function
  | Zoom1 -> 1, 1
  | Zoom2 _ -> 4, 4
  | Zoom3 _ -> 8, 8
  | Zoom4 -> 16, 16

  (* How much we need to divide the normal screen coordinates by *)
let tile_div_of_zoom = function
  | Zoom1 -> 16
  | Zoom2 _ -> 4
  | Zoom3 _ -> 2
  | Zoom4 -> 1

let tile_textures_of_zoom s = function
  | Zoom3 _ | Zoom2 _ -> s.State.textures.small_tiles
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
  let start_x_map = start_x * C.tile_w in
  let start_y_map = start_y * C.tile_h in
  let end_x_map = end_x * C.tile_w in
  let end_y_map = end_y * C.tile_h in
  start_x, start_y, end_x, end_y, start_x_map, start_y_map, end_x_map, end_y_map

let minimap_bounds v ~(minimap:Utils.rect) =
  let start_x_tile = Utils.clip (v.center_x - minimap.w/2) ~min:0 ~max:(v.dims.w - minimap.w) in
  let start_y_tile = Utils.clip (v.center_y - minimap.h/2) ~min:0 ~max:(v.dims.h - minimap.h) in
  let end_x_tile = start_x_tile + minimap.w in
  let end_y_tile = start_y_tile + minimap.h in
  start_x_tile, start_y_tile, end_x_tile, end_y_tile

let check_recenter_zoom4 v cursor_x cursor_y =
  (* recenter in zoom4 if past screen, but only in given direction *)
  let tile_w, tile_h = tile_size_of_zoom Zoom4 in
  let start_x, start_y, end_x, end_y, _, _, _, _ = mapview_bounds v tile_w tile_h in
  if (cursor_y > 0 && cursor_y < start_y + 1)
    || (cursor_y < v.dims.h - 1 && cursor_y >= end_y - 1)
    || (cursor_x > 0 && cursor_x < start_x + 1)
    || (cursor_x < v.dims.w - 1 && cursor_x >= end_x - 1) then
      cursor_x, cursor_y
  else
    v.center_x, v.center_y

let move_const_box v dir n =
  let dx, dy = Dir.to_offsets dir in
  let x, y = v.const_box_x + dx * n, v.const_box_y + dy * n in
  let const_box_x = Utils.clip x ~min:0 ~max:(v.dims.w-1) in
  let const_box_y = Utils.clip y ~min:0 ~max:(v.dims.h-1) in
  let center_x, center_y = check_recenter_zoom4 v const_box_x const_box_y in
  {v with const_box_x; const_box_y; center_x; center_y}

let set_const_box_to_loc v ~x ~y =
  let const_box_x = Utils.clip x ~min:0 ~max:(v.dims.w-1) in
  let const_box_y = Utils.clip y ~min:0 ~max:(v.dims.h-1) in
  let center_x, center_y = check_recenter_zoom4 v const_box_x const_box_y in
  {v with const_box_x; const_box_y; center_x; center_y}

  (* Used by menu *)
let const_box_on_woodbridge ?cursor_x ?cursor_y backend v =
  let cursor_x = Option.get_or ~default:v.const_box_x cursor_x in
  let cursor_y = Option.get_or ~default:v.const_box_y cursor_y in
  match B.get_track cursor_x cursor_y backend with
  | Some track when Owner.(track.player = C.player) ->
      begin match track.kind with
      | Bridge Wood -> true
      | _ -> false
      end
  | _ -> false

let const_box_on_station ?cursor_x_tile ?cursor_y_tile ?(all=false) backend v =
  (* all: all stations, not just Depot and higher *)
  let cursor_x_tile = Option.get_or ~default:v.const_box_x cursor_x_tile in
  let cursor_y_tile = Option.get_or ~default:v.const_box_y cursor_y_tile in
  (* check if we're clicking on a station *)
  match B.get_track cursor_x_tile cursor_y_tile backend with
  | Some track when Owner.(track.player = C.player) ->
      begin match track.kind with
      | Station (`Depot | `Station | `Terminal) -> true
      | Station `SignalTower when all -> true
      | _ -> false
      end
  | _ -> false

let const_box_on_signal backend tile_x tile_y ~dx ~dy =
  (* assumes we are on a station *)
  (* offsets: delta relative to tile *)
  let midpoint_x, midpoint_y = C.tile_dim/2, C.tile_dim/2 in
  let delta_x, delta_y = dx - midpoint_x, dy - midpoint_y in
  match B.get_track backend tile_x tile_y with
  | Some {kind=Station _; player; dirs; _} when Owner.(player = C.player) ->
    Dir.Set.find_opt dirs (function
      | Up when delta_x > 0 && delta_y < 0 -> true
      | UpRight when delta_x > 0 && delta_y >= -2 && delta_y <= 2 -> true
      | Right when delta_x > 0 && delta_y > 0 -> true
      | DownRight when delta_y > 0 && delta_x >= -2 && delta_x <= 2 -> true
      | Down when delta_y > 0 && delta_x < 0 -> true
      | DownLeft when delta_x < 0 && delta_y >= -2 && delta_y <= 2 -> true
      | Left when delta_y < 0 && delta_x < 0 -> true
      | UpLeft when delta_y < 0 && delta_x >= -2 && delta_x <= 2 -> true
      | _ -> false)
  | _ -> None

let get_station_under_cursor_exn backend v =
  (* get the station the cursor is over *)
  match B.get_station (v.const_box_x, v.const_box_y) backend with
  | Some station -> station
  | None -> failwith "No station under cursor"

let set_build_mode v mode = {v with build_mode = mode}

let get_build_mode v = v.build_mode

let get_survey v = v.survey

let set_survey b v = {v with survey=b}

let update_option option value v =
  let options = 
    if value then
      Options.add option v.options 
    else
      Options.remove option v.options
  in
  {v with options}

let flush_draw_buffer v =
  (* Called whenever the zoom2/3 view is changed *)
  Hashtbl.clear v.draw_buffer;
  ()

let set_zoom zoom v =
  {v with zoom; survey=false}

let handle_event (s:State.t) (v:t) (event:Event.t) ~(minimap:Utils.rect) =
  let handle_mapview_button v x y button =
    let offset_x, offset_y = v.dims.x - 1, v.dims.y - 1 in
    let tile_w, tile_h = tile_size_of_zoom v.zoom in
    let tile_div = tile_div_of_zoom v.zoom in
    let start_x_tile, start_y_tile, _, _, start_x_map, start_y_map, end_x_map, end_y_map = mapview_bounds v tile_w tile_h in
    let screen_tile_x = (x - v.dims.x) / tile_w in
    let screen_tile_y = (y - v.dims.y) / tile_h in
    let cursor_x_tile = start_x_tile + screen_tile_x |> Utils.clip ~min:0 ~max:(v.dims.w - 1) in
    let cursor_y_tile = start_y_tile + screen_tile_y |> Utils.clip ~min:0 ~max:(v.dims.h - 1) in
    let is_in_view x y =
      (* In map coordinates *)
      x >= start_x_map - C.draw_margin && y >= start_y_map - C.draw_margin &&
      x <= end_x_map + C.draw_margin && y <= end_y_map + C.draw_margin
    in
    let to_screen_pxls x y =
      (* convert from map pixels to screen pixels *)
      let x = (x - start_x_map)/tile_div + offset_x in
      let y = (y - start_y_map)/tile_div + offset_y in
      x, y
    in
    let cursor_on_train ~car_pixels ~dist cursor_x cursor_y =
      (* cursor_x, y in terms of screen pixels *)
      let test_loc x y =
        if is_in_view x y then
          let screen_x, screen_y = to_screen_pxls x y in
          Utils.classic_dist (screen_x, screen_y) (cursor_x, cursor_y) <= dist
        else
          false
      in
      let trains = B.get_trains C.player s.backend in
      Trainmap.find_ret_index (fun train ->
        (* Test cars *)
        let on_car = 
          Utils.List.findi (fun i _ ->
            let x, y, _ = Train.calc_car_loc s.backend.track ~car_pixels i train in
            test_loc x y
          ) train.cars
        in
        let on_engine () =
          let x, y, _ = Train.calc_car_loc_in_pixels s.backend.track 0 train in
          test_loc x y
        in
        Option.is_some on_car || on_engine ()
      ) trains
    in
    begin match v.zoom, button with
    | Zoom4, `Left when cursor_x_tile = v.const_box_x && cursor_y_tile = v.const_box_y ->
        (* second click, after focusing the cursor on that tile *)
        if const_box_on_station s.backend v then
          let screen_x, screen_y = to_screen_pxls (cursor_x_tile * C.tile_w) (cursor_y_tile * C.tile_h) in
          match const_box_on_signal cursor_x_tile cursor_y_tile s.backend ~dx:(x - screen_x) ~dy:(y - screen_y) with
          | Some dir -> (* signal click *)
            v, `SignalMenu (cursor_x_tile, cursor_y_tile, dir, x, y)
          | None -> (* station click *)
            v, `StationView (cursor_x_tile, cursor_y_tile)
        else begin match cursor_on_train x y ~car_pixels:12 ~dist:10 with
          | Some train_idx -> v, `EditTrain train_idx
          | _ -> (* tile click *)
            let tile = B.get_tile cursor_x_tile cursor_y_tile s.backend in
            v, `ShowTileInfo (cursor_x_tile, cursor_y_tile, tile)
          end
    | Zoom4, `Left ->
        (* move cursor *)
        let center_x, center_y = check_recenter_zoom4 v cursor_x_tile cursor_y_tile in
        {v with center_x; center_y; const_box_x=cursor_x_tile; const_box_y=cursor_y_tile}, `NoAction

      (* Zoom_station is open *)
    | (Zoom3 {zoom_station=Some _} | Zoom2 {zoom_station=Some _}), (`Right | `Left) ->
        with_zoom_23 v (fun _ -> {zoom_station=None}), `NoAction

    | (Zoom4 | Zoom3 _ | Zoom2 _), `Right ->
        (* recenter *)
        flush_draw_buffer v;
        {v with center_x=cursor_x_tile; center_y=cursor_y_tile; const_box_x=cursor_x_tile; const_box_y=cursor_y_tile}, `NoAction
    | (Zoom3 _ | Zoom2 _), `Left ->
        let show_stationbox = Options.mem v.options `StationBoxes in
        let stationbox_click = Tilebuffer.get_loc v.tile_buffer screen_tile_x screen_tile_y in
        begin match stationbox_click with
        | Some (station_x, station_y) when show_stationbox ->
            let x_tile, y_tile = station_x + start_x_tile, station_y + start_y_tile in
            v, `StationView (x_tile, y_tile)
        | _ when const_box_on_station s.backend v ~cursor_x_tile ~cursor_y_tile ~all:true ->
            (* station click *)
            with_zoom_23 v (fun _ -> {zoom_station=Some(cursor_x_tile, cursor_y_tile)}), `NoAction
        | _ ->
            let click_on_train = cursor_on_train x y ~car_pixels:8 ~dist:2 in
            begin match click_on_train with
            | Some train_idx -> v, `EditTrain train_idx
            | _ ->
              (* tile info *)
              let tile = B.get_tile cursor_x_tile cursor_y_tile s.backend in
              v, `ShowTileInfo (cursor_x_tile, cursor_y_tile, tile)
            end
        end
    | _ -> v, `NoAction
    end
  in
  let handle_mouse_button v x y button =
    match v.zoom with
      | Zoom1 ->
          let y = y - v.dims.y in
          {v with center_x=x; center_y=y; const_box_x=x; const_box_y=y; zoom=def_zoom2}, `NoAction
      | _ when x > minimap.x && y > minimap.y && y < minimap.y + minimap.h ->
          (* click on minimap *)
          let start_x_tile, start_y_tile, _, _ = minimap_bounds v ~minimap in
          let x = x - minimap.x + start_x_tile in
          let y = y - minimap.y + start_y_tile in
          flush_draw_buffer v;
          {v with center_x=x; center_y=y; const_box_x=x; const_box_y=y}, `NoAction
      | _ when x <= v.dims.x + v.dims.w && y > v.dims.y ->
          handle_mapview_button v x y button
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
  let player_idx = C.player in

  let handle_key_zoom4 v key ~build =
    match key_to_dir key with
    | Some dir ->
        let move i = move_const_box v dir i in
        let msg () = Backend.Action.{x=v.const_box_x; y=v.const_box_y; dir; player_idx} in
        if build then (
          if v.build_mode then
            (* Build track *)
            match B.check_build_track (v.const_box_x, v.const_box_y) ~dir player_idx s.backend with
            | `Ok | `RateWar _ -> move 1, `BuildTrack(msg ())
            | `Ferry -> move 1, `BuildFerry(msg ())
            | `HighGrade g -> v, `HighGradeTrack(msg (), g, false)
            | `Bridge -> v, `BuildBridge(msg ()) 
            | `Tunnel g -> v, `HighGradeTrack(msg (), g, true)
            | `Illegal -> v, `NoAction
          else
            (* Remove Track *)
            match B.check_remove_track (v.const_box_x, v.const_box_y) ~dir player_idx s.backend with
            | true -> move 1, `RemoveTrack(msg ())
            | false -> v, `NoAction
        ) else
          (* Regular movement *)
          move 1, `NoAction
    | None ->
      match key with
      | Event.Enter ->
          begin match B.get_station (v.const_box_x, v.const_box_y) s.backend with
          | Some station when Station.is_proper_station station ->
            v, `StationView (v.const_box_x, v.const_box_y)
          | _ ->
            let tile = B.get_tile v.const_box_x v.const_box_y s.backend in
            v, `ShowTileInfo (v.const_box_x, v.const_box_y, tile)
          end
      | Event.K1 when build &&
            B.check_change_double_track (v.const_box_x, v.const_box_y) player_idx ~double:false s.backend ->
              v, `DoubleTrack(false, v.const_box_x, v.const_box_y, player_idx)
      | Event.K2 when build &&
            B.check_change_double_track (v.const_box_x, v.const_box_y) player_idx ~double:true s.backend ->
              v, `DoubleTrack(true, v.const_box_x, v.const_box_y, player_idx)
      | _ -> v, `NoAction
  in

  let v, actions =
    match event with
    | Key {down=true; key=F1; _} ->
        set_zoom Zoom1 v, `NoAction
    | Key {down=true; key=F2; _} ->
        flush_draw_buffer v;
        set_zoom def_zoom2 v, `NoAction
    | Key {down=true; key=F3; _} ->
        flush_draw_buffer v;
        set_zoom def_zoom3 v, `NoAction
    | Key {down=true; key=F4; _} ->
        set_zoom Zoom4 v, `NoAction
    | MouseButton {down=true; x; y; button; _} ->
        handle_mouse_button v x y button
    | Key {down=true; key; modifiers; _} when is_zoom4 v ->
        let build = Event.Modifiers.shift modifiers in
        handle_key_zoom4 v key ~build
    | _ -> v, `NoAction
  in
  v, actions

let render win (s:State.t) (v:t) ~minimap ~build_station =
  let b = s.backend in
  let region = b.params.region in
  let player_idx = C.player in
  let tile_w, tile_h = tile_size_of_zoom v.zoom in
  let tile_div = tile_div_of_zoom v.zoom in
  let tile_w2, tile_h2 = tile_w/2, tile_h/2 in
  (* In tile coordinates: where our view starts *)
  let start_x, start_y, end_x, end_y, start_x_map, start_y_map, end_x_map, end_y_map = mapview_bounds v tile_w tile_h in
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
  let draw_tile ~tile_x ~tile_y ~screen_x ~screen_y ~zoom =
    let tiles = tile_textures_of_zoom s zoom in
    (* Check for alternate tile *)
    let alt = ((tile_x + tile_y) land 1) > 0 in
    let tile = B.get_tile tile_x tile_y b in
    let tex = Textures.TileTex.find tiles ~region:(B.get_region s.backend) ~alt tile in
    R.Texture.render win tex ~x:screen_x ~y:screen_y
  in
  let draw_resource ~tile_x ~tile_y ~screen_x ~screen_y ~zoom =
    let idx, offset_x, offset_y = match zoom with
      | Zoom2 _ -> `Tiny, 1, 0
      | Zoom3 _ -> `Caps, 2, 1
      | _ -> assert false in
    let write = Fonts.Render.write win s.fonts ~idx in
    let tile = B.get_tile tile_x tile_y b in
    let info = Tile.Info.get region tile in
    let x, y = screen_x + offset_x, screen_y + offset_y in
    match Tile.Info.resource_map_supply_demand info with
    | `Supply good ->
        let letter = Goods.show good |> String.take 1 in
        let color = Freight.of_good good |> Freight.to_color ~full:true in
        write ~x ~y ~color letter
    | `Demand good ->
        let color = Freight.of_good good |> Freight.to_color ~full:true in
        R.draw_rect win ~x:screen_x ~y:screen_y ~w:tile_w ~h:tile_h ~fill:true ~color;
        let letter = Goods.show good |> String.take 1 in
        write ~x ~y ~color:Ega.white letter
    | `None -> ();
  in
  let draw_tiles () =
    iter_screen (fun x y ->
      let tile_x, tile_y = start_x + x, start_y + y in
      let screen_x, screen_y = v.dims.x + x * tile_w, v.dims.y + y * tile_h in
      draw_tile ~tile_x ~tile_y ~screen_x ~screen_y ~zoom:v.zoom
    )
  in
  let draw_resource_map () =
    iter_screen (fun x y ->
      let tile_x, tile_y = start_x + x, start_y + y in
      let screen_x, screen_y = v.dims.x + x * tile_w, v.dims.y + y * tile_h in
      draw_resource ~tile_x ~tile_y ~screen_x ~screen_y ~zoom:v.zoom
    )
  in
  let draw_city_names () =
    B.iter_cities (fun tile_x tile_y (name,_) ->
      if tile_x >= start_x && tile_y >= start_y && tile_x <= end_x && tile_y <= end_y then (
        let x = (tile_x - start_x) * tile_w in
        let y = (tile_y - start_y) * tile_h + v.dims.y in
        let x, y = x + 11, y - 15 in
        Fonts.Render.write win s.fonts name ~idx:`Standard ~x ~y ~color:Ega.black;
        let x, y = x + 1, y - 1 in
        Fonts.Render.write win s.fonts name ~idx:`Standard ~x ~y ~color:Ega.bcyan;
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
    (* Draw trains *)
    let trains = B.get_trains C.player s.backend in
    Trainmap.iter (fun train ->
      (* NOTE: Is the 2nd black point necessary? *)
      List.iter (fun (pixels, color) ->
        let x_map, y_map, _ = Train.calc_car_loc_in_pixels s.backend.track pixels train in
        (* Must use tile_dim: tile_div changes by zoom *)
        let x_tile, y_tile = x_map / C.tile_dim, y_map / C.tile_dim in
        if x_tile >= from_x && x_tile <= from_x_end && y_tile >= from_y && y_tile <= from_y_end then (
          let x, y = start_x + x_tile - from_x, start_y + y_tile - from_y in
          R.draw_point win ~color ~x ~y
        ))
      [0, Ega.white; 16, Ega.black]
    ) trains
  in
  let bridge_washout = B.get_player player_idx s.backend |> Player.get_bridge_washout in
  let draw_track_zoom4 ~tile_x ~tile_y ~screen_x ~screen_y =
    (* draw an individual piece of tile *)
    let track_h = s.State.textures.tracks in
    let misc_h = s.textures.misc in
    match B.get_track tile_x tile_y s.backend, bridge_washout with
    | Some track, _ when Track.is_visually_double track ->
      let tex = Textures.Tracks.find track_h track in
      let (x1, y1), (x2, y2) = Track.double_track_offsets track in
      let xd, yd = screen_x + x1 - 2, screen_y + y1 - 2 in
      R.Texture.render win tex ~x:xd ~y:yd;
      let xd, yd = screen_x + x2 - 2, screen_y + y2 - 2 in
      R.Texture.render win tex ~x:xd ~y:yd

    | Some ({kind=Station station_kind; dirs;_} as track), _ ->
      let tex = Textures.Tracks.find track_h track in
      R.Texture.render win tex ~x:(screen_x-2) ~y:(screen_y-2);

      (* draw signals *)
      let light_h = s.State.textures.station_lights in
      let station = Station_map.get_exn (tile_x, tile_y) s.backend.stations in
      Dir.Set.iter (fun dir ->
        let has_connection_in_dir = 
          let tile_x2, tile_y2 = Dir.adjust dir tile_x tile_y in
          match B.get_track tile_x2 tile_y2 s.backend with
          | Some track when Track.has_dir track ~dir:(Dir.opposite dir) -> true
          | _ -> false
        in
        if has_connection_in_dir then (
          let color = Station.get_signal station dir |> Station.color_of_signal in
          let tex = Textures.StationLights.find light_h (dir, station_kind) in
          R.Texture.render ~color win tex ~x:(screen_x-2) ~y:(screen_y-2))
      ) dirs

    | Some ({kind=Bridge _;_} as track), Some(x,y) when tile_x=x && tile_y=y ->
      let tex = Textures.Tracks.find track_h track in
      R.Texture.render win tex ~x:(screen_x-2) ~y:(screen_y-2);
      let water_tex = Hashtbl.find misc_h `WaterOnBridge in
      R.Texture.render win water_tex ~x:(screen_x-2) ~y:(screen_y-2)
      
    | Some track, _ ->
      let tex = Textures.Tracks.find track_h track in
      R.Texture.render win tex ~x:(screen_x-2) ~y:(screen_y-2)

    | _ -> ()
  in
  let draw_tracks_zoom2_3 mult zoom_station =
    iter_screen @@ fun x y ->
      let tile_x, tile_y = start_x + x, start_y + y in
      match B.get_track tile_x tile_y s.backend with
      | Some track ->
        let x = v.dims.x + x * tile_w + tile_w2 in
        let y = v.dims.y + y * tile_h + tile_h2 in

        let draw_signals () =
          let station = Loc_map.get_exn (tile_x, tile_y) s.backend.stations in
          let mult = mult + 1 in
          Dir.Set.iter (fun dir ->
            (* Only draw signal if we have track in this direction *)
            let tile_x, tile_y = Dir.adjust dir tile_x tile_y in
            if Option.is_some @@ B.get_track tile_x tile_y s.backend then (
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
            );
          ) track.dirs
        in
        let draw_zoom4_station () =
          match zoom_station with
          | Some (tile_x2, tile_y2) when tile_x = tile_x2 && tile_y = tile_y2 ->
              draw_tile ~tile_x ~tile_y ~screen_x:(x-8) ~screen_y:(y-8) ~zoom:Zoom4;
              draw_track_zoom4 ~tile_x ~tile_y ~screen_x:(x-8) ~screen_y:(y-8);
              R.draw_rect win ~x:(x-9) ~y:(y-9) ~w:18 ~h:18 ~fill:false ~color:Ega.white
          | _ -> ()
        in
        let draw_normal_track ~color ~is_double dirs =
          let draw_segment x y dir =
            let dx, dy = Dir.to_offsets dir in
            R.draw_line win ~color ~x1:x ~y1:y
              ~x2:(x+dx*tile_w2) ~y2:(y+dy*tile_h2);
          in
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
          dirs
        in
        begin match track.kind, bridge_washout with
        | Station (`Depot | `Station | `Terminal), _ ->
            (* Draw station outline *)
            R.draw_rect win ~color:Ega.white ~x:(x-3) ~y:(y-3) ~w:6 ~h:6 ~fill:true;
            draw_signals ();
            draw_zoom4_station ()

        | Station `SignalTower, _ ->
            draw_signals ();
            draw_zoom4_station ()

        | Bridge _, Some (bridge_x,bridge_y) when bridge_x = tile_x && bridge_y = tile_y ->
            (* Draw an X on the brige *)
            draw_normal_track ~color:Ega.bred ~is_double:true Dir.Set.x_shape

        | _normal_track, _ ->
            let is_double = Track.acts_like_double track in
            draw_normal_track ~color:Ega.white ~is_double track.dirs
        end
      | _ -> ()
  in
  let draw_trains_zoom2_3 () =
    let offset_x, offset_y = v.dims.x - 1, v.dims.y - 1 in
    let draw_car_or_engine color x y =
      if is_in_view x y then (
        let x = (x - start_x_map)/tile_div + offset_x in
        let y = (y - start_y_map)/tile_div + offset_y in
        R.draw_rect win ~x ~y ~w:2 ~h:2 ~color ~fill:true;
      )
    in
    (* draw old trains from previous frames *)
    Hashtbl.iter (fun _ train_history ->
      for i=Array.length train_history - 1 downto 1 do
        List.iter (fun (x, y, color) -> draw_car_or_engine color x y) train_history.(i)
      done)
    v.draw_buffer;

    (* draw current trains *)
    let trains = B.get_trains player_idx s.backend in
    Trainmap.iteri (fun train_num train ->
      let should_write_to_buffer =
        match Hashtbl.find_opt v.draw_buffer train_num with
        | Some history when List.length history.(0) = 0 -> true
        | None -> true (* create an entry for the train *)
        | _ -> false
      in
      let set_draw_buffer draw_buffer =
        Hashtbl.update v.draw_buffer
        ~f:(fun _ arr -> match arr with
          | Some arr ->
              arr.(0) <- draw_buffer;
              Some arr
          | None ->
              let arr = Array.make C.draw_buffer_len [] in
              arr.(0) <- draw_buffer;
              Some arr)
        ~k:train_num
      in
      (* Draw cars *)
      let draw_buffer = 
        List.foldi (fun draw_list i car ->
          let x, y, _ = Train.calc_car_loc_in_pixels s.backend.track ((i+1)*8) train in
          let color = Train.Car.get_freight car |> Freight.to_color ~full:true in
          draw_car_or_engine color x y;
          if should_write_to_buffer then (x,y,color)::draw_list else []
        ) [] train.cars
      in
      (* Draw engine *)
      let x, y, _ = Train.calc_car_loc_in_pixels s.backend.track 0 train in
      draw_car_or_engine Ega.black x y;
      let draw_buffer =
        if should_write_to_buffer then (x,y,Ega.black)::draw_buffer else []
      in
      if should_write_to_buffer then set_draw_buffer draw_buffer;
    ) trains;
  in
  let draw_stationboxes mult size =
    (* mult and size are in tiles! *)
    Tilebuffer.clear v.tile_buffer;
    let copy_to_tile_buffer () =
      iter_screen @@ fun x y ->
        let tile_x, tile_y = start_x + x, start_y + y in
        if Option.is_some @@ B.get_track tile_x tile_y s.backend then
          Tilebuffer.set v.tile_buffer x y ~value:(-1)
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
    let draw_stationbox station loc s_tile_x s_tile_y tile_x tile_y =
      (* tiles in terms of on-screen tiles
         s_tiles: station tiles
       *)
      let station_x, station_y = s_tile_x * tile_w + v.dims.x + tile_w2, s_tile_y * tile_h + v.dims.y + tile_w2 in
      (* Mark box in buffer *)
      let x, y = tile_x * tile_w + v.dims.x, tile_y * tile_w + v.dims.y in
      (* draw to halfway point of box *)
      R.draw_line win ~x1:(x+16) ~y1:(y+16) ~x2:station_x ~y2:station_y ~color:Ega.white;
      R.draw_rect win ~x ~y ~w:32 ~h:32 ~fill:true ~color:Ega.bblue;
      let revenue = Station.total_goods_revenue station in
      let revenue = M.clip revenue ~min:0 ~max:(64 * 30) |> M.to_int in
      let h = revenue / 64 in
      R.draw_rect win ~x ~y:(y+32-h) ~w:32 ~h ~fill:true ~color:Ega.bgreen; (* frame *)
      let y_line = y + 32 - h - 1 in
      (* Draw final line *)
      R.draw_line win ~x1:x ~y1:y_line ~x2:(x + revenue mod 32) ~y2:y_line ~color:Ega.bgreen;
      (* Draw demand lines and supply cars *)
      let demand = Station.get_demand_exn station in
      let supply = Station.get_supply_exn station in
      List.iter (fun good ->
        let n_freight = Freight.of_good good |> Freight.to_enum in
        let n_good = Freight.idx_of_good good in
        let x = x + n_good * 10 in
        let y = y + n_freight * 5 + 10 in
        if Goods.Set.mem good demand then (
          R.draw_line win ~x1:x ~y1:y ~x2:(x+10) ~y2:y ~color:Ega.dgray);
        let y = y - 2 in
        let x = x + 2 in
        Hashtbl.get supply good
        |> Option.iter (fun amount ->
          let cars = (amount + C.car_amount / 2) / C.car_amount in
          if cars >= 3 then (
            Ui_common.draw_ui_car win ~x ~y:(y-1) ~full:false good);
          if cars >= 4 then (
            Ui_common.draw_ui_car win ~x:(x+5) ~y:(y-1) ~full:false good);
          if cars >= 1 then (
            Ui_common.draw_ui_car win ~x ~y ~full:true good);
          if cars >= 2 then (
            Ui_common.draw_ui_car win ~x:(x+5) ~y ~full:true good);
        )) @@
        Goods.of_region (B.get_region s.backend)
      |> ignore;
      (* frame *)
      let color = Station.color_of_rates station in
      R.draw_rect win ~x ~y ~w:32 ~h:32 ~fill:false ~color;

      (* Priority shipment *)
      let priority =
        let player_idx = Station.get_player_idx station in
        B.get_player player_idx s.backend |> Player.get_priority
      in
      Option.iter (fun priority ->
        let draw_letter letter =
          let color = Freight.to_color ~full:true priority.Priority_shipment.freight in
          Fonts.Render.write win s.fonts ~color letter ~x ~y:(y+2) ~idx:`Standard
        in
        if Utils.equal_loc priority.src_loc loc then draw_letter "P"
        else if Utils.equal_loc priority.dst_loc loc then draw_letter "D"
        else ()) 
        priority;

      (* station name *)
      let name = Station.get_short_name station in
      Fonts.Render.write win s.fonts ~color name ~x:(x+7) ~y:(y+2) ~idx:`Standard;
    in
    iter_screen @@ fun x y ->
      let (tile_x, tile_y) as loc = start_x + x, start_y + y in
      if tile_x >= start_x && tile_x < end_x - size &&
         tile_y >= start_y && tile_y < end_y - size then (
        B.get_track tile_x tile_y s.backend 
        |> Option.iter (fun track ->
          if Track.is_big_station track then
            let station = Station_map.get_exn loc s.backend.stations in
            let box_x, box_y = find_space_for_stationbox x y in
            Tilebuffer.set_box v.tile_buffer ~x:box_x ~y:box_y ~w:size ~h:size x y;
            draw_stationbox station loc x y box_x box_y
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
    let x = (v.const_box_x - start_x) * tile_w in
    let y = (v.const_box_y - start_y) * tile_h + v.dims.y in
    let color = if v.build_mode then Ega.white else Ega.red in
    R.draw_rect win ~x ~y ~w:tile_w ~h:tile_h ~color ~fill:false
  in
  let draw_tracks_zoom4 () =
    iter_screen (fun x y ->
      let tile_x, tile_y = start_x + x, start_y + y in
      let screen_x, screen_y = v.dims.x + x * tile_w, v.dims.y + y * tile_h in
      draw_track_zoom4 ~tile_x ~tile_y ~screen_x ~screen_y
    )
  in
  let draw_trains_zoom4 () =
    let open Textures.CarsTop in
    let offset_x, offset_y = (-C.tile_w/2) - 2, v.dims.y -C.tile_h/2 - 2 in
    Trainmap.iter (fun train ->
      (* Draw cars *)
      List.iteri (fun i car ->
        let car_x, car_y, car_dir =
          Train.calc_car_loc ~car_pixels:12 s.backend.track i train 
        in
        if is_in_view car_x car_y then (
          let freight = Freight.of_good car.Train.Car.good in
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
    (B.get_player C.player s.backend |> Player.get_trains);
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
      match B.get_tile map_x map_y s.backend with
      | Tile.Ocean _ -> ()
      | _ ->
        let height = (B.get_tile_height map_x map_y s.backend ) / 2 |> string_of_int in
        let x, y = x * tile_w + 4 + v.dims.x, y * tile_h + 4 + v.dims.y in
        Fonts.Render.write win s.fonts height ~idx:`Tiny ~x ~y ~color:Ega.white
    )
  in
  let draw_buildstation_mode () =
    let color = Ega.gray in
    let fill = false in
    let font = Fonts.get_font `Standard s.fonts in
    let x = (v.const_box_x - start_x - 1) * tile_w in
    let font_x = x in
    let y = (v.const_box_y - start_y - 1) * tile_h + v.dims.y in
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

  let draw_cyan_background () =
    R.draw_rect win ~x:0 ~y:v.dims.y ~w:v.dims.w ~h:v.dims.h ~color:Ega.cyan ~fill:true;
  in

  begin match v.zoom with
  | Zoom1 ->
      R.Texture.render win s.map_tex ~x:0 ~y:v.dims.y;
      draw_track_and_trains_zoom1 0 0 v.dims.w v.dims.h v.dims.x v.dims.y
  | Zoom2 st ->
      draw_cyan_background ();
      if Options.mem v.options `Resources then (
        draw_resource_map ()
      );
      if Options.mem v.options `StationBoxes then (
        draw_stationboxes 6 8
      );
      draw_tracks_zoom2_3 1 st.zoom_station;
      draw_trains_zoom2_3 ();
      draw_minimap ~minimap
  | Zoom3 st ->
      if Options.mem v.options `Resources then (
        draw_cyan_background ();
        draw_resource_map ()
      ) else (
        draw_tiles ()
      );
      if Options.mem v.options `StationBoxes then (
        draw_stationboxes 3 4
      );
      draw_tracks_zoom2_3 2 st.zoom_station;
      draw_trains_zoom2_3 ();
      draw_minimap ~minimap
  | Zoom4 ->
      draw_tiles ();
      draw_city_names ();
      if build_station then (
        draw_buildstation_mode ())
      else if v.survey then (
        draw_survey_zoom4 ()
      );
      draw_tracks_zoom4 ();
      draw_trains_zoom4 ();
      draw_minimap ~minimap;
      draw_cursor_zoom4 ();
  end;
  s

let handle_tick (s:State.t) (v:t) _time is_cycle =
 (* We only run by backend cycles *)
  let player_idx = C.player in
  if not is_cycle then v
  else
  (* Move smoke *)
  let smoke_plumes =
    if s.backend.params.cycle mod 3 = 0 then (
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
      let _, _, _, _, start_x_map, start_y_map, end_x_map, end_y_map = mapview_bounds v tile_w tile_h in
      (* Create plumes of smoke *)
      let smoke_plumes =
        Trainmap.foldi (fun i acc train ->
          if Engine.has_steam train.engine &&
             Train.get_speed train > 0 &&
             (Train.Id.to_int i * 3 + s.backend.params.cycle) mod 16 = 0 &&
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
        (B.get_trains player_idx s.backend)
      in
      smoke_plumes
    | _ -> smoke_plumes
  in
  [%upf v.smoke_plumes <- smoke_plumes];
  let () =
    match v.zoom with
    | Zoom2 _ | Zoom3 _ ->
      (* Cycle the draw buffer for any trains that have new data *)
      Hashtbl.iter (fun _ arr ->
        if not @@ equal_train_history arr.(0) arr.(1) then (
          (* The order is critical here *)
          for i=Array.length arr - 1 downto 1 do
            arr.(i) <- arr.(i-1)
          done;
        );
        arr.(0) <- []
    ) v.draw_buffer
    | _ -> ()
  in
  v


