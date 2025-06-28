open! Containers

module R = Renderer
module B = Backend
module M = Money
module C = Constants

let sp = Printf.sprintf

let render job win (s:State.t) =
  let region = s.backend.params.region in
  R.paint_screen win ~color:Ega.white;
  R.draw_rect win ~x:2 ~y:2 ~w:(320-4) ~h:(200-4) ~color:Ega.black ~fill:false;

  let _draw_pic =
    let tex = Hashtbl.find s.textures.jobs job in
    let x, y = 314 - tex.w, 195 - tex.h in
    R.Texture.render win ~x ~y tex;
  in

  



  ()
