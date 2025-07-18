
type mode =
  | Animation of Pani_render.t
  | TransitionScreen of Transition.t
  | GenericScreen of {render_fn: Renderer.window -> unit}

type t = {
  mode: mode;
  next_modes: mode list;
}

