open Containers
open Mapview_d

let default = {center_x=0; center_y=0; zoom=Zoom4; width=Gmap.map_width; height=Gmap.map_height}

let tile_size_of_zoom = function
  | Zoom1 -> 1, 1
  | Zoom2 | Zoom3 -> 8, 8
  | Zoom4 -> 16, 16

let tile_textures_of_zoom s = function
  | Zoom3 | Zoom2 -> s.State.textures.small_tiles
  | Zoom4 -> s.textures.tiles
  | Zoom1 -> failwith "tile_textures_of_zoom"

  (* For drawing code. Make sure we don't exceed map limits *)
let calc_start_bounds_check v tile_w tile_h =
  (* Pay attention to end as well *)
  let x_delta = v.width/(tile_w*2) in
  let y_delta = v.height/(tile_h*2) in
  let start_x = Utils.clip (v.center_x - x_delta) 0 (Gmap.map_width - 2 * x_delta) in
  let start_y = Utils.clip (v.center_y - y_delta) 0 (Gmap.map_height - 2 * y_delta) in
  let end_x = start_x + 2 * x_delta in
  let end_y = start_y + 2 * y_delta in
  start_x, start_y, end_x, end_y

  (* Only care about start position. For mouse *)
let calc_start v tile_w tile_h =
  let x_delta = v.width/(tile_w*2) in
  let y_delta = v.height/(tile_h*2) in
  let start_x = v.center_x - x_delta |> max 0 in
  let start_y = v.center_y - y_delta |> max 0 in
  start_x, start_y

let update (s:State.t) (v:t) (event:Event.t) =
  begin match event with
  | Key {down=true; key=F1; _} ->
      v.zoom <- Zoom1
  | Key {down=true; key=F2; _} ->
      v.zoom <- Zoom2
  | Key {down=true; key=F3; _} ->
      v.zoom <- Zoom3
  | Key {down=true; key=F4; _} ->
      v.zoom <- Zoom4
  | MouseButton {down=true; x; y; _} ->
      begin match v.zoom with
      | Zoom1 ->
          v.center_x <- x;
          v.center_y <- y
      | _ ->
        let tile_w, tile_h = tile_size_of_zoom v.zoom in
        let start_x, start_y = calc_start v tile_w tile_h in
        let x = start_x + x/tile_w in
        let y = start_y + y/tile_h in
        v.center_x <- x;
        v.center_y <- y;
      end
  | _ -> ()
  end;
  s


module R = Renderer

let render win (s:State.t) (v:t) =
  let y_ui = Ui.mapview_start s.ui in

  let tile_w, tile_h = tile_size_of_zoom v.zoom in
  let start_x, start_y, end_x, end_y = calc_start_bounds_check v tile_w tile_h in

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

  let draw_minimap () =
    let x = s.ui.dims.ui_start_x in
    let y = s.ui.dims.menu_h in
    let h = s.ui.dims.minimap_h in
    let w = s.ui.dims.ui_w in
    let from_x = Utils.clip (v.center_x - w/2) ~min:0 ~max:(Gmap.map_width - w) in
    let from_y = Utils.clip (v.center_y - h/2) ~min:0 ~max:(Gmap.map_height - h) in
    R.Texture.render_subtex win s.textures.map ~x ~y ~from_x ~from_y ~w ~h;
    let x = s.ui.dims.ui_start_x + start_x - from_x in
    let y = s.ui.dims.menu_h + start_y - from_y in
    R.draw_rect win ~x ~y ~w:(end_x - start_x + 1) ~h:(end_y - start_y + 1) ~color:Ega.white
      ~fill:false;
  in

  R.clear_screen win;

  begin match v.zoom with
  | Zoom1 ->
      R.Texture.render win s.textures.map ~x:0 ~y:y_ui;
      Ui.render win s s.ui ~draw_logo:true;
  | Zoom2 | Zoom3 ->
      tile_render ();
      draw_minimap ();
      Ui.render win s s.ui ~draw_logo:false;
  | Zoom4 ->
      tile_render ();
      draw_city_names ();
      draw_minimap ();
      Ui.render win s s.ui ~draw_logo:false;
  end;

  s

