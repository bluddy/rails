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
  let x = 16 in
  let y = 17 in
  Fonts.Render.write win s.fonts ~color:Ega.gray ~x ~y src_txt;
  let txt, method_ = Clue.get_text s clue_id case in
  let txt = Utils.add_newlines ~width:40 txt in
  let y = y + 8 in
  let _, h = Fonts.get_w_h s.fonts txt in
  Fonts.Render.write win s.fonts ~color:Ega.black ~x ~y txt;
  let y = y + h in
  let method_ = match clue.src with
    | _ when Org.Id.(clue.org = Org.cia) -> method_
    | Wiretap -> Clue.Method.Wiretap
    | _ -> Clue.Method.Photo
  in
  let txt = Printf.sprintf "Method: %s" (Clue.Method.show method_) in
  Fonts.Render.write win s.fonts ~color:Ega.gray ~x ~y txt;
  begin match clue.connect with
    | Clue.Connect.Face agent_id ->
        let face = Agent.Map.find agent_id (Case.G.agents case) |> Agent.G.face in
        Face.render_photo win s face 264 18;
        let tex = Hashtbl.find s.textures.images `Paper_clip in
        R.Texture.render ~x:299 ~y:46 win tex
    | _ ->
        let tex = Hashtbl.find s.textures.clue_methods method_ in
        R.Texture.render ~x:272 ~y:16 win tex
  end;
  Fonts.Render.write win s.fonts ~color:Ega.red ~x:16 ~y:66 "Related Clues:";
  let same_name, connects =
    Clue.Map.fold (fun clue_id2 clue2 ((same_name, connects) as acc) ->
      if Clue.Id.(clue_id = clue_id2) then acc else
      if clue.id = clue2.Clue.id then (clue_id2::same_name, connects) else
      if Clue.Connect.(clue.connect = clue2.connect) then (same_name, clue_id2::connects) else
      acc)
    (Case.G.clues case)
    ([], [])
  in
  let draw_frame x y =
    R.draw_rect win ~x ~y ~w:304 ~h:(200 - y) ~color:Ega.white ~fill:true;
    let x2 = 304 + x in
    R.draw_line win ~x1:(x+1) ~y1:y ~x2 ~y2:y ~color:Ega.gray;
    R.draw_line win ~x1:x2 ~y1:y ~x2 ~y2:199 ~color:Ega.dgray;
    R.draw_line win ~x1:x ~y1:(y+1) ~x2:x ~y2:199 ~color:Ega.gray;
  in
  if List.is_empty same_name && List.is_empty connects then
    Fonts.Render.write win s.fonts ~color:Ega.black ~x:16 ~y:72 "...none" else

  List.fold_left (fun clue_id (y, y_section) ->
    let x = Random.int_range 4 16 s.random in (* TODO: Must move to state *)
    draw_frame x (y-2);
    let clue = Clue.Map.find clue_id (Case.G.clues case) in
    let text =
      if Clue.is_connect_role clue then
        Clue.get_text s clue_id case |> fst
      else
        Clue.get_summary_text s clue_id case
    in
    ()
  )
  (76, 73)



  ()

