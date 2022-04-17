open Containers
open Tsdl

open Mapview_d

let width = 256
let height = 192

let default = {center_x=0; center_y=0; zoom=F4}

let update (s:State.t) (v:t) (event:Sdl.event option) =
  begin match event with
  | Some event ->
      begin match Event.typ event with
      | `Key_down ->
          begin match Event.key event with
          | F1 -> v.zoom <- F1
          | F2 -> v.zoom <- F2
          | F3 -> v.zoom <- F3
          | F4 -> v.zoom <- F4
          | _ -> ()
          end
      | _ -> ()
      end
  | _ -> ()
  end;
  s


module R = Renderer

(* TODO: alt tiles *)
let render win (s:State.t) (v:t) =
  let do_render tile_w tile_h tiles =
    let tile_w = 16 in
    let tile_h = 16 in
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
  | F1 ->
    R.clear_screen win;
    R.render win s.textures.map;
    s
  | F4 ->
      do_render 16 16 s.textures.tiles;
      s
  | F2 | F3 ->
      do_render 8 8 s.textures.small_tiles;
      s

