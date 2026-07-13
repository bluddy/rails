open! Containers

module R = Engine.Renderer
module Ega = Engine.Ega
module Event = Engine.Event

(* Screen to view a clue *)

type t = unit

let render_folder win (s:Services.t) color text =
  R.paint_screen win ~color:Ega.brown;
  R.draw_rect win ~x:0 ~y:10 ~w:320 ~h:190 ~color ~fill:true;
  R.draw_rect win ~x:160 ~y:2 ~w:140 ~h:8 ~color ~fill:true;
  R.draw_line win ~x1:160 ~y1:1 ~x2:307 ~y2:1 ~color:Ega.black; (* top line on tab *)
  R.draw_line win ~x1:0 ~y1:10 ~x2:160 ~y2:10 ~color:Ega.dgray; (* top line on left *)
  R.draw_line win ~x1:318 ~y1:10 ~x2:319 ~y2:10 ~color:Ega.dgray; (* top line on left *)
  let left = Hashtbl.find s.textures.images `Left_file in
  let right = Hashtbl.find s.textures.images `Right_file in
  R.Texture.render ~x:160 ~y:2 win left;
  R.Texture.render ~x:307 ~y:2 win right;
  R.draw_rect win ~x:8 ~y:14 ~w:304 ~h:186 ~color:Ega.white ~fill:true;
  R.draw_line win ~x1:10 ~y1:13 ~x2:312 ~y2:13 ~color:Ega.gray;
  R.draw_line win ~x1:312 ~y1:13 ~x2:312 ~y2:199 ~color:Ega.dgray;
  let w, _ = Fonts.get_w_h s.fonts text in
  R.draw_rect win ~x:(239-w/2) ~y:3 ~w:(w+2) ~h:9 ~color:Ega.gray ~fill:true;
  Fonts.Render.write win s.fonts ~color:Ega.black ~x:(240-w/2) ~y:4 text;
  ()

let render_clue win (s:Services.t) (case:Case.t) clue_id =
  render_folder win s Ega.bblue "Clue";
  let clue = Clue.Map.find clue_id @@ Case.G.clues case in
  let org = Org.Map.find clue.org @@ Case.G.orgs case in
  let loc = Loc.Map.find clue.loc @@ Case.G.locs case in
  let src_txt = Printf.sprintf "Source: %s/%s" org.name loc.city in
  let y = 17 in
  Fonts.Render.write win s.fonts ~color:Ega.gray ~x:16 ~y src_txt;
  let txt, method_, cur_agent_face = Clue.get_text s clue_id case in
  let txt = Utils.add_newlines ~width:42 txt in
  let y = y + 8 in
  let _, h = Fonts.get_w_h s.fonts txt in
  Fonts.Render.write win s.fonts ~color:Ega.black ~x:16 ~y txt;
  let y = y + h in








let create_clue (s:Services.t) clue_id (case:Case.t) = ()


  


