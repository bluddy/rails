open Containers
module R = Renderer
module B = Backend

type t = {
  index: int;
}

let make index =
  { index }

let render win s v =
  ()

let handle_event s v event =
  v, B.Action.NoAction
