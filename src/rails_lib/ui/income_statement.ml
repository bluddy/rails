open! Containers

let render win (s:State.t) =
  Ui_common.render_full_screen_frame win s.textures s.ui.dims;
  ()


