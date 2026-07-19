open! Containers

module R = Engine.Renderer
module Ega = Engine.Ega

let find_and_render win (s:Services.t) gender part idx ~x ~y =
  let tex = Hashtbl.find s.textures.face_parts (gender, part, idx) in
  R.Texture.render win tex ~x ~y

let draw_neck win (s:Services.t) (face:Agent.Face.t) x y =
  find_and_render win s face.gender `Neck face.neck ~x ~y

let draw_face win (s:Services.t) (face:Agent.Face.t) x y =
  let render = find_and_render win s face.gender ~x ~y in
  render `Head face.head;
  render `Mouth face.mouth;
  render `Nose face.nose;
  render `Eyes face.eyes;
  render `Hair face.hair;
  ()

let render_photo win (s:Services.t) ?(with_clip=false) (face:Agent.Face.t) x y =
  R.draw_rect win ~x:(x-2) ~y:(y-2) ~w:54 ~h:56 ~color:Ega.gray ~fill:false;
  R.draw_rect win ~x:(x-1) ~y:(y-1) ~w:52 ~h:54 ~color:Ega.white ~fill:true;
  R.draw_rect win ~x:(x+2) ~y:(y+2) ~w:46 ~h:45 ~color:Ega.black ~fill:false;
  let bg_color = if (face.id / 4) land 3 = 3 then Ega.gray else Ega.cyan in
  R.draw_rect win ~x:(x+3) ~y:(y+3) ~w:44 ~h:43 ~color:bg_color ~fill:true;
  draw_face win s face (x+13) (y+3);
  draw_neck win s face (x+3) (y+28);
  if with_clip then begin
    let tex = Hashtbl.find s.textures.images `Paper_clip in
    R.Texture.render ~x:(x+35) ~y:(y+28) win tex
  end;
  ()



