open! Containers

module R = Renderer
module B = Backend
module M = Money

let render win fonts (b:Backend.t) =
  R.paint_screen win ~color:Ega.white;
  R.draw_rect win ~x:2 ~y:2 ~w:(320-4) ~h:(200-4) ~color:Ega.black ~fill:false;
  Fonts.Render.write win ~idx:2 ~x:108 ~y:4 ~color:Ega.black fonts "Rate War!";
  let write = Fonts.Render.write win ~idx:4 fonts in
  let write_b = write ~color:Ega.blue in
  let write_r = write ~color:Ega.red in
  let write = write ~color:Ega.black in
  write ~x:120 ~y:20 "in %s"



  ()
