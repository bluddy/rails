
type mode =
  | Animation of Pani_render.t
  | TransitionScreen of Transition.t
  | GenericScreen of {render_fn: Renderer.window -> unit; timeout: int option}

type t = {
  mode: mode;
  next_modes: mode list;
}

