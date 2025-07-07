open! Containers

module R = Renderer
module B = Backend
module C = Constants
module M = Money

type mode =
  | Animation of Pani_render.t
  | GenericScreen of {render_fn: Renderer.window -> State.t -> unit}
  | Menu

type t = {
  mode: mode;
  next_modes: mode list;
}

let set_modes l v = match l with
  | x::xs -> {mode=x; next_modes=xs}
  | _ -> v

let render_screen tex win _s =
  R.Texture.render win ~x:0 ~y:0 tex

let create (s:State.t) =
  let add_screen tex mode =
    let tex = Hashtbl.find s.textures tex in
    GenericScreen{render_fn=render_screen tex}::mode in
  let mode = [] in
  let mode = add_screen `LogoMicroprose mode in
  let mode = add_screen `LogoMPS mode in
  let mode = add_screen `Council mode in
  let mode =
    let file = "intro.pan" in
    Animation(Pani_render.create file)::mode in
{
  mode;
  next_modes=[];
}

let render win v =
  ()

let handle_event win v event =
  ()

let handle_tick win v time =
  ()
