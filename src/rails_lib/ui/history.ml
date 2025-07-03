  open! Containers

include History_d

let render win _v (s:State.t) =
  Ui_common.render_full_screen_frame win s.textures s.ui.dims;
  ()
