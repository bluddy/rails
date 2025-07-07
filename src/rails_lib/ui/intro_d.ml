
type 'state mode =
  | Animation of Pani_render.t
  | GenericScreen of {render_fn: Renderer.window -> 'state -> unit}

type 'state t = {
  mode: 'state mode;
  next_modes: 'state mode list;
}

