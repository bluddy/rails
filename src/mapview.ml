open Containers
open Mapview_d

let default = {center_x=0; center_y=0; zoom=Zoom4; width=256; height=192}

let tile_size_of_zoom = function
  | Zoom1 -> 1, 1
  | Zoom2 | Zoom3 -> 8, 8
  | Zoom4 -> 16, 16

let tile_textures_of_zoom s = function
  | Zoom3 | Zoom2 -> s.State.textures.small_tiles
  | Zoom4 -> s.textures.tiles
  | Zoom1 -> failwith "tile_textures_of_zoom"

let calc_start v tile_w tile_h =
  let start_x = v.center_x - v.width/(tile_w*2) in
  let start_x = if start_x < 0 then 0 else start_x in
  let start_y = v.center_y - v.height/(tile_h*2) in
  let start_y = if start_y < 0 then 0 else start_y in
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
      let tile_w, tile_h = tile_size_of_zoom v.zoom in
      let start_x, start_y = calc_start v tile_w tile_h in
      let x = start_x + x/tile_w in
      let y = start_y + y/tile_h in
      v.center_x <- x;
      v.center_y <- y;
  | _ -> ()
  end;
  s


module R = Renderer

(* TODO: alt tiles *)
let render win (s:State.t) (v:t) =
  let do_render tile_w tile_h tiles =
    let start_x, start_y = calc_start v tile_w tile_h in

    let open Iter in
    iter (fun i ->
      iter (fun j ->
        let tile = Gmap.get_tile s.game.map (start_x+j) (start_y+i) in
        let tex = Textures.Tile.find tiles ~area:s.game.area ~alt:false tile in
        let x, y = j * tile_w, i * tile_h in
        R.render win tex ~x ~y;
      )
      (0--(v.width/tile_w - 1)))
    (0--(v.height/tile_h - 1))
  in
  R.clear_screen win;
  begin match v.zoom with
  | Zoom1 ->
      R.render win s.textures.map;
  | _ ->
      let tile_w, tile_h = tile_size_of_zoom v.zoom in
      let tiles = tile_textures_of_zoom s v.zoom in
      do_render tile_w tile_h tiles;
  end;
  s

