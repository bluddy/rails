open! Containers

type mode =
  | Animation of Pani_render.t
  | GenericScreen {render_fn: Renderer.window -> unit}
  | Menu

type t = {
  mode: mode;
  next_mode: mode;
}
