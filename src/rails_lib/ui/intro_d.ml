
type mode =
  | Animation of Engine.Pani_render.t
  | TransitionScreen of Engine.Transition.t
  | GenericScreen of {render_fn: Engine.Renderer.window -> unit; timeout: int option}

type t = {
  mode: mode;
  next_modes: mode list;
}

