open Containers
open Mapview_d

(* The mapview changes fairly slowly and can thefore be mutated functionally *)
let default = 
  {cursor_x=0; cursor_y=0;
  center_x=0; center_y=0;
  zoom=Zoom4;
  width=Gmap.map_width;
  height=Gmap.map_height}

let recenter_threshold = 5

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
  let x_delta = v.width/(tile_w*2) in
  let y_delta = v.height/(tile_h*2) in
  let start_x = Utils.clip (v.center_x - x_delta) ~min:0 ~max:(v.width - 2 * x_delta) in
  let start_y = Utils.clip (v.center_y - y_delta + 1) ~min:0 ~max:(v.height - 2 * y_delta) in
  let end_x = start_x + 2 * x_delta in
  let end_y = start_y + 2 * y_delta in
  start_x, start_y, end_x, end_y

let minimap_bounds v w h =
  let start_x = Utils.clip (v.center_x - w/2) ~min:0 ~max:(v.width - w) in
  let start_y = Utils.clip (v.center_y - h/2) ~min:0 ~max:(v.height - h) in
  start_x, start_y

let update (s:State.t) (v:t) (event:Event.t) ~y_top ~minimap_x ~minimap_y ~minimap_w ~minimap_h =

  let check_recenter cursor_x cursor_y =
    (* recenter in zoom4 if past screen, but only in given direction *)
    let tile_w, tile_h = tile_size_of_zoom Zoom4 in
    let start_x, start_y, end_x, end_y = mapview_bounds v tile_w tile_h in
    if (v.cursor_y > 0 && v.cursor_y < start_y + 1)
       || (v.cursor_y < v.height - 1 && v.cursor_y >= end_y - 1)
       || (v.cursor_x > 0 && v.cursor_x < start_x + 1)
       || (v.cursor_x < v.width - 1 && v.cursor_x >= end_x - 1) then
         cursor_x, cursor_y
    else
         v.center_x, v.center_y
  in

  let handle_mouse_button x y =
      begin match v.zoom with
      | Zoom1 ->
          let y = y - y_top in
          {v with center_x=x; center_y=y; cursor_x=x; cursor_y=y; zoom=Zoom4}
      | _ ->
          (* minimap *)
          if x > minimap_x && y > minimap_y && y < minimap_y + minimap_h then (
            let start_x, start_y = minimap_bounds v minimap_w minimap_h in
            let x = x - minimap_x + start_x in
            let y = y - minimap_y + start_y in
            {v with center_x=x; center_y=y; cursor_x=x; cursor_y=y}
          ) else (
            (* Mapview *)
            let tile_w, tile_h = tile_size_of_zoom v.zoom in
            let start_x, start_y, _, _ = mapview_bounds v tile_w tile_h in
            let x = start_x + x/tile_w |> Utils.clip ~min:0 ~max:(v.width - 1) in
            let y = start_y + (y-y_top)/tile_h |> Utils.clip ~min:0 ~max:(v.height - 1) in
            begin match v.zoom with
            | Zoom4 ->
              let cursor_x, cursor_y = check_recenter x y in
              {v with cursor_x; cursor_y}
            | _ ->
              {v with cursor_x=x; cursor_y=y; center_x=x; center_y=y}
            end
          )
      end
  in
  let v =
    match event with
    | Key {down=true; key=F1; _} ->
        {v with zoom = Zoom1}
    | Key {down=true; key=F2; _} ->
        {v with zoom = Zoom2}
    | Key {down=true; key=F3; _} ->
        {v with zoom = Zoom3}
    | Key {down=true; key=F4; _} ->
        {v with zoom = Zoom4}
    | MouseButton {down=true; x; y; _} ->
        handle_mouse_button x y
    | Key {down=true; key; _} when equal_zoom v.zoom Zoom4 ->
        let x, y =
          let x, y = v.cursor_x, v.cursor_y in
          match key with
          | Q -> x-1, y-1
          | W | Up -> x, y-1
          | E -> x+1, y-1
          | A | Left -> x-1, y
          | D | Right -> x+1, y
          | Z -> x-1, y+1
          | S | Down -> x, y+1
          | C -> x+1, y+1
          | _ -> x, y
        in
        let x = Utils.clip x ~min:0 ~max:v.width-1 in
        let y = Utils.clip y ~min:0 ~max:v.height-1 in
        let cursor_x, cursor_y = check_recenter x y in
        {v with cursor_x; cursor_y}
    | _ -> v
  in
  s.view <- v;
  s

module R = Renderer

let render win (s:State.t) (v:t) ~y ~minimap_x ~minimap_y ~minimap_h ~minimap_w =
  let y_ui = y in

  let tile_w, tile_h = tile_size_of_zoom v.zoom in
  let start_x, start_y, end_x, end_y = mapview_bounds v tile_w tile_h in

  let tile_render () =
    let tiles = tile_textures_of_zoom s v.zoom in

    Iter.iter (fun i ->
      Iter.iter (fun j ->
        let map_x, map_y = start_x + j, start_y + i in
        let alt = ((map_x + map_y) land 1) > 0 in
        let tile = Gmap.get_tile s.game.map (start_x+j) (start_y+i) in
        let tex = Textures.Tile.find tiles ~area:s.game.area ~alt tile in
        let x, y = j * tile_w, y_ui + i * tile_h in
        R.Texture.render win tex ~x ~y;
      )
      Iter.(0--(v.width/tile_w - 1)))
    Iter.(0--(v.height/tile_h - 1))
  in

  let draw_city_names () =
    Array.iter (fun {Gmap.name; x; y} ->
      if (x >= start_x && y >= start_y) || (x <= end_x && y <= end_y) then (
        let x = (x - start_x) * tile_w in
        let y = (y - start_y) * tile_h + y_ui in
        let x, y = x + 11, y - 15 in
        Fonts.Render.write win s.textures.fonts name ~x ~y ~color:Ega.black;
        let x, y = x + 1, y - 1 in
        Fonts.Render.write win s.textures.fonts name ~x ~y ~color:Ega.bcyan;
      )
    )
    s.game.cities
  in

  let draw_minimap x y w h =
    let from_x, from_y = minimap_bounds v w h in
    R.Texture.render_subtex win s.textures.map ~x ~y ~from_x ~from_y ~w ~h;

    (* minimap rectangle *)
    let x = x + start_x - from_x in
    let y = y + start_y - from_y in
    R.draw_rect win ~x ~y ~w:(end_x - start_x + 1) ~h:(end_y - start_y + 1) ~color:Ega.white
      ~fill:false
  in

  let draw_cursor () =
    let x = (v.cursor_x - start_x) * tile_w in
    let y = (v.cursor_y - start_y) * tile_h + y_ui in
    R.draw_rect win ~x ~y ~w:tile_w ~h:tile_h ~color:Ega.white ~fill:false
  in

  R.clear_screen win;

  begin match v.zoom with
  | Zoom1 ->
      R.Texture.render win s.textures.map ~x:0 ~y:y_ui;
  | Zoom2 | Zoom3 ->
      tile_render ();
      draw_minimap minimap_x minimap_y minimap_w minimap_h;
  | Zoom4 ->
      tile_render ();
      draw_city_names ();
      draw_minimap minimap_x minimap_y minimap_w minimap_h;
      draw_cursor ();
  end;

  s

let get_zoom v = v.zoom
