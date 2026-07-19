open! Containers

module R = Engine.Renderer
module Ega = Engine.Ega
module Event = Engine.Event

(* Screen to view a clue *)

type related = {
  clue_id: Clue.Id.t;
  clue: Clue.t;
  x_offset: int;
  text: string
}

type t = {
  clue_id: Clue.Id.t;
  clue: Clue.t;
  text: string;
  method_: Clue.Method.t;
  related: related list;
  case: Case.t;
}

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

let create_clue (s:Services.t) (case:Case.t) clue_id =
  let clue = Clue.Map.find clue_id @@ Case.G.clues case in
  let text, method_ = Clue.get_text s clue_id case in
  let text = Utils.add_newlines ~width:40 text in
  let method_ = match clue.src with
    | _ when Org.Id.(clue.org = Org.cia) -> method_
    | Wiretap -> Clue.Method.Wiretap
    | _ -> Clue.Method.Photo
  in
  let same_name, connects =
    Clue.Map.fold (fun clue_id2 clue2 ((same_name, connects) as acc) ->
      if Clue.Id.(clue_id = clue_id2) then acc else
      if clue.id = clue2.Clue.id then (clue_id2::same_name, connects) else
      if Clue.Connect.(clue.connect = clue2.connect) then (same_name, clue_id2::connects) else
      acc)
    (Case.G.clues case)
    ([], [])
  in
  let related = same_name @ connects in
  let related =
    List.map (fun clue_id ->
      let x_offset = Random.int_range 4 16 s.random in
      let clue = Clue.Map.find clue_id @@ Case.G.clues case in
      let text = match clue.connect with
        | Clue.Connect.Role _ ->
          Clue.get_text s clue_id case |> fst
        | _ ->
          Clue.get_summary_text_non_role clue_id case
      in
      let text = Utils.add_newlines text in
      {x_offset; clue_id; clue; text})
    related
  in
  {method_; clue_id; clue; text; related; case}


let render_clue win (s:Services.t) v =
  render_folder win s Ega.bblue "Clue";
  let clue = v.clue in
  let org = Org.Map.find clue.org @@ Case.G.orgs v.case in
  let loc = Loc.Map.find clue.loc @@ Case.G.locs v.case in
  let src_txt = Printf.sprintf "Source: %s/%s" org.name loc.city in
  let x = 16 in
  let y = 17 in
  Fonts.Render.write win s.fonts ~color:Ega.gray ~x ~y src_txt;
  let txt = Utils.add_newlines ~width:40 v.text in
  let y = y + 8 in
  let _, h = Fonts.get_w_h s.fonts txt in
  Fonts.Render.write win s.fonts ~color:Ega.black ~x ~y txt;
  let y = y + h in
  let txt = Printf.sprintf "Method: %s" (Clue.Method.show v.method_) in
  Fonts.Render.write win s.fonts ~color:Ega.gray ~x ~y txt;
  begin match clue.connect with
    | Clue.Connect.Face agent_id ->
        let face = Agent.Map.find agent_id (Case.G.agents v.case) |> Agent.G.face in
        Face.render_photo ~with_clip:true win s face 264 18
    | _ ->
        let tex = Hashtbl.find s.textures.clue_methods v.method_ in
        R.Texture.render ~x:272 ~y:16 win tex
  end;
  Fonts.Render.write win s.fonts ~color:Ega.red ~x:16 ~y:66 "Related Clues:";
  let draw_frame x y =
    R.draw_rect win ~x ~y ~w:304 ~h:(200 - y) ~color:Ega.white ~fill:true;
    let x2 = 304 + x in
    R.draw_line win ~x1:(x+1) ~y1:y ~x2 ~y2:y ~color:Ega.gray;
    R.draw_line win ~x1:x2 ~y1:y ~x2 ~y2:199 ~color:Ega.dgray;
    R.draw_line win ~x1:x ~y1:(y+1) ~x2:x ~y2:199 ~color:Ega.gray;
  in
  if List.is_empty v.related then
    Fonts.Render.write win s.fonts ~color:Ega.black ~x:16 ~y:72 "...none" else

  let _draw_related_clues =
    List.fold_left (fun ((y, y_pics) as acc) r ->
      if y > 180 then acc else (
      draw_frame r.x_offset (y-2);
      let clue = r.clue in
      Fonts.Render.write win s.fonts ~color:Ega.black ~x:(x+8) ~y r.text;
      let _, h = Fonts.get_w_h s.fonts r.text in
      let y = y + h + 4 in
      let y_pics = match clue.connect with
        | Clue.Connect.Face agent_id ->
            let y_pics' = Utils.clip y_pics ~min:0 ~max:150 in
            let agent = Agent.Map.find agent_id @@ Case.G.agents v.case in
            Face.render_photo ~with_clip:true win s agent.face 264 y_pics';
            y_pics + 40
        | _ -> y_pics
      in
      y, y_pics))
    (76, 73)
    v.related
  in
  ()

