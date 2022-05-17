open Containers
open Mapview_d
module B = Backend

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
    build_mode=`Build;
    survey=false;
    options=Options.of_list [`StationBoxes];
  }

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

let move_cursor v dir =
  let dx, dy = Dir.to_offsets dir in
  let x, y = v.cursor_x + dx, v.cursor_y + dy in
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
  match B.get_track backend v.cursor_x v.cursor_y with
  | Some track when track.player = 0 ->
      begin match track.kind with
      | Station (Depot | Station | Terminal) -> true
      | _ -> false
      end
  | _ -> false

let set_build_mode v mode =
  {v with build_mode = mode}

let get_build_mode v = v.build_mode

let get_survey v = v.survey

let set_survey v b = {v with survey=b}

let handle_event (s:State.t) (v:t) (event:Event.t) ~(minimap:Utils.rect) =

  let handle_mouse_button v x y =

    match v.zoom with
      | Zoom1 ->
          let y = y - v.dims.y in
          {v with center_x=x; center_y=y; cursor_x=x; cursor_y=y; zoom=Zoom4}
      | _ when x > minimap.x && y > minimap.y && y < minimap.y + minimap.h ->
          (* click on minimap *)
          let start_x, start_y, _, _ = minimap_bounds v ~minimap in
          let x = x - minimap.x + start_x in
          let y = y - minimap.y + start_y in
          {v with center_x=x; center_y=y; cursor_x=x; cursor_y=y}
      | _ when x < minimap.x && y > v.dims.y ->
          (* click in mapview *)
          let tile_w, tile_h = tile_size_of_zoom v.zoom in
          let start_x, start_y, _, _ = mapview_bounds v tile_w tile_h in
          let cursor_x = start_x + x/tile_w |> Utils.clip ~min:0 ~max:(v.dims.w - 1) in
          let cursor_y = start_y + (y-v.dims.y)/tile_h |> Utils.clip ~min:0 ~max:(v.dims.h - 1) in
          let center_x, center_y =
            match v.zoom with
            | Zoom4 ->
                check_recenter_zoom4 v cursor_x cursor_y
            | _ ->
                v.center_x, v.center_y
          in
          {v with center_x; center_y; cursor_x; cursor_y}
       | _ -> v
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
    match dir with
    | None -> v, `NoAction
    | Some dir ->
        let v2 = move_cursor v dir in
        (* TODO: handle track removal, bridge, tunnel etc *)
        if build then
          let check = B.check_build_track s.backend ~x:v.cursor_x ~y:v.cursor_y ~dir ~player:0 in
          match check with
          | `Ok ->
              let action =
                `BuildTrack Utils.{x=v.cursor_x; y=v.cursor_y; dir; player=0}
              in
              v2, action
          | `Bridge ->
              let action = 
                `BuildBridge Utils.{x=v.cursor_x; y=v.cursor_y; dir; player=0}
              in
              v2, action
          | `Illegal ->
              v, `NoAction
        else
          v2, `NoAction
  in

  let v, actions =
    match event with
    | Key {down=true; key=F1; _} ->
        {v with zoom = Zoom1}, `NoAction
    | Key {down=true; key=F2; _} ->
        {v with zoom = Zoom2}, `NoAction
    | Key {down=true; key=F3; _} ->
        {v with zoom = Zoom3}, `NoAction
    | Key {down=true; key=F4; _} ->
        {v with zoom = Zoom4}, `NoAction
    | MouseButton {down=true; x; y; _} ->
        handle_mouse_button v x y, `NoAction
    | Key {down=true; key; modifiers; _} when equal_zoom v.zoom Zoom4 ->
        let build = Event.Modifiers.shift modifiers in
        handle_key_zoom4 v key ~build
    | _ -> v, `NoAction
  in
  v, actions

module R = Renderer

let render win (s:State.t) (v:t) ~minimap ~build_station =
  let tile_w, tile_h = tile_size_of_zoom v.zoom in
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
      let tex = Textures.Tile.find tiles ~area:(B.get_area s.backend) ~alt tile in
      let x, y = j * tile_w, v.dims.y + i * tile_h in
      R.Texture.render win tex ~x ~y;
    )
  in

  let draw_city_names () =
    B.iter_cities (fun name x y ->
      if (x >= start_x && y >= start_y) || (x <= end_x && y <= end_y) then (
        let x = (x - start_x) * tile_w in
        let y = (y - start_y) * tile_h + v.dims.y in
        let x, y = x + 11, y - 15 in
        Fonts.Render.write win s.textures.fonts name ~idx:4 ~x ~y ~color:Ega.black;
        let x, y = x + 1, y - 1 in
        Fonts.Render.write win s.textures.fonts name ~idx:4 ~x ~y ~color:Ega.bcyan;
      )
    )
    s.backend
  in

  let draw_track_zoom1 () =
    B.trackmap_iter s.backend (fun x y _ ->
      R.draw_point win ~x ~y:(y + v.dims.y) ~color:Ega.black
    )
  in

  let draw_minimap ~(minimap:Utils.rect) =
    let from_x, from_y, from_end_x, from_end_y = minimap_bounds v ~minimap in
    R.Texture.render_subtex win s.textures.map ~x:minimap.x ~y:minimap.y
      ~from_x ~from_y ~w:minimap.w ~h:minimap.h;

    (* draw track *)
    B.trackmap_iter s.backend (fun x y _ ->
      if x >= from_x && x <= from_end_x && y >= from_y && y <= from_end_y then (
        let x = minimap.x + x - from_x in
        let y = minimap.y + y - from_y in
        R.draw_point win ~x ~y:(y + v.dims.y) ~color:Ega.black
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
    R.draw_rect win ~x ~y ~w:tile_w ~h:tile_h ~color:Ega.white ~fill:false
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

  let draw_survey_zoom4 () =
    iter_screen (fun i j ->
      let map_x, map_y = start_x + j, start_y + i in
      match B.get_tile s.backend map_x map_y with
      | Gmap.Ocean _ -> ()
      | _ ->
        let height = (B.get_tile_height s.backend map_x map_y) / 2 |> string_of_int in
        let x, y = j * tile_w + 4, i * tile_h + 4 + v.dims.y in
        Fonts.Render.write win s.textures.fonts height ~idx:3 ~x ~y ~color:Ega.white
    )
  in

  let draw_buildstation_mode () =
    let color = Ega.gray in
    let fill = false in
    let font = Fonts.get_font s.textures.fonts 4 in
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
      R.Texture.render win s.textures.map ~x:0 ~y:v.dims.y;
      draw_track_zoom1 ()
  | Zoom2 | Zoom3 ->
      tile_render ();
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
      draw_cursor_zoom4 ();
  end;
  s

