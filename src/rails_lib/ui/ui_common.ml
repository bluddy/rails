open! Containers

module R = Renderer
module C = Constants

let draw_ui_car win ~x ~y ~full good =
  let freight = Freight.of_good good in
  let color = Freight.to_color freight ~full in
  let freight_idx = Freight.idx_of_good good in
  R.draw_rect win ~x ~y ~w:4 ~h:2 ~color ~fill:true;
  R.draw_point win ~x ~y:(y+2) ~color:Ega.black;
  R.draw_point win ~x:(x+3) ~y:(y+2) ~color:Ega.black;
  (* Draw background holes for certain classes *)
  match freight_idx with
  | 1 ->
    let y = y + 1 in
    R.draw_line win ~x1:(x+1) ~y1:y ~x2:(x+2) ~y2:y ~color:Ega.bblue
  | 2 ->
    R.draw_line win ~x1:(x+1) ~y1:y ~x2:(x+2) ~y2:y ~color:Ega.bblue
  | _ -> ()

let render_full_screen_frame win textures (dims:Main_ui_d.dims) =
  R.paint_screen win ~color:Ega.white;
  R.draw_rect win ~x:2 ~y:2 ~w:(dims.screen.w - 4) ~h:(dims.screen.h - 4) ~color:Ega.dgray ~fill:false;
  match
    List.map (fun dir -> Hashtbl.find textures.Textures.misc dir)
      [`FrameBL; `FrameBR; `FrameTL; `FrameTR]
  with
  | [bl; br; tl; tr] ->
    R.Texture.render ~x:4 ~y:4 win tl;
    R.Texture.render ~x:(dims.screen.w - 12) ~y:4 win tr;
    R.Texture.render ~x:4 ~y:(dims.screen.h - 12) win bl;
    R.Texture.render ~x:(dims.screen.w - 12) ~y:(dims.screen.h - 12) win br;
    ()
  | _ -> assert false

