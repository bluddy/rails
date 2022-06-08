open Containers
module R = Renderer

(* Station view screen *)

let render win s station =
  let w, h = R.width win, R.height win in
  R.draw_rect win ~fill:true ~x:0 ~y:0 ~w ~h ~color:Ega.cyan;
  ()

