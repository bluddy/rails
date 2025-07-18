
type mode =
  | Animation of Pani_render.t
  | GenericScreen of {
    render_fn: Renderer.window -> unit;
    end_transition: bool;
    transition: Transition.t option;
  }

type t = {
  mode: mode;
  next_modes: mode list;
}

