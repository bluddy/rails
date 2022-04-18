open Containers
open Tsdl

open Mapview_d

let width = 256
let height = 192

let default = {center_x=0; center_y=0; zoom=Zoom4}

let update (s:State.t) (v:t) (event:Event.t) =
  begin match event with
  | Key {down=true; key=F1; _} -> v.zoom <- Zoom1
  | Key {down=true; key=F2; _} -> v.zoom <- Zoom2
  | Key {down=true; key=F3; _} -> v.zoom <- Zoom3
  | Key {down=true; key=F4; _} -> v.zoom <- Zoom4
  | MouseButton {down=true; x; y} ->
      v.center_x <- x;
      v.center_y <- y
  | _ -> ()
  end;
  s


module R = Renderer

(* TODO: alt tiles *)
let render win (s:State.t) (v:t) =
  let do_render tile_w tile_h tiles =
    let start_x = v.center_x - width/(tile_w*2) in
    let start_x = if start_x < 0 then 0 else start_x in
    let start_y = v.center_y - height/(tile_h*2) in
    let start_y = if start_y < 0 then 0 else start_y in

    let open Iter in
    iter (fun i ->
      iter (fun j ->
        let tile = Gmap.get_tile s.game.map (start_x+j) (start_y+i) in
        let tex = Textures.Tile.find tiles ~area:s.game.area ~alt:false tile in
        let x, y = j * tile_w, i * tile_h in
        R.render win tex ~x ~y;
      )
      (0--(width/tile_w - 1)))
    (0--(height/tile_h - 1))
  in
  match v.zoom with
  | Zoom1 ->
    R.clear_screen win;
    R.render win s.textures.map;
    s
  | Zoom4 ->
      do_render 16 16 s.textures.tiles;
      s
  | Zoom2 | Zoom3 ->
      do_render 8 8 s.textures.small_tiles;
      s

