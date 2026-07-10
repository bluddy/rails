open! Containers

module R = Engine.Renderer
module Ega = Engine.Ega
module Event = Engine.Event

(* Screen to view a clue *)

type t = unit

let render_folder win (s:Services.t) color text =
  R.paint_screen win ~color:Ega.brown;
  R.draw_rect win ~x:0 ~y:10 ~w:320 ~h:190 ~color ~fill:true;
  R.draw_rect win ~x:8 ~y:14 ~w:304 ~h:186 ~color:Ega.white ~fill:true;
  R.draw_line win ~x1:10 ~y1:13 ~x2:312 ~y2:13 ~color:Ega.gray;
  R.draw_line win ~x1:312 ~y1:13 ~x2:312 ~y2:199 ~color:Ega.dgray;
  let w, _ = Fonts.get_w_h s.fonts text in
  R.draw_rect win ~x:(239-w/2) ~y:3 ~w:(w+2) ~h:9 ~color:Ega.gray ~fill:true;
  Fonts.Render.write win s.fonts ~color:Ega.black ~x:(240-w/2) ~y:4 text;
  ()


let create (s:Services.t) clue_id (case:Case.t) = ()
  


