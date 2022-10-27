open Containers
module R = Renderer
module B = Backend

type t = {
  index: int;
}

let nobaction = B.Action.NoAction

let make index = {
  index
}

let render win _s _v =
  R.paint_screen win ~color:Ega.white;
  ()

let handle_event _s v event =
  if Event.pressed_esc event then
    true, v, nobaction
  else
    false, v, nobaction
