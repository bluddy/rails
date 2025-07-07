open! Containers

module R = Renderer
module B = Backend
module C = Constants
module M = Money

include Main_menu_d

let set_modes l v = match l with
  | x::xs -> {mode=x; next_modes=xs}
  | _ -> v

let render_screen tex win _s =
  R.Texture.render win ~x:0 ~y:0 tex

let next_mode v = match v.next_modes with
  | _::_ -> `None, set_modes v.next_modes v
  | [] -> `Exit, v

let create (s:State.t) =
  let add_screen tex mode =
    let tex = Hashtbl.find s.textures.misc tex in
    GenericScreen{render_fn=render_screen tex}::mode in
  let mode = [] in
  let mode = add_screen `LogoMicroprose mode in
  let mode = add_screen `LogoMPS mode in
  let mode = add_screen `Council mode in
  let mode =
    let file = "intro.pan" in
    Animation(Pani_render.create file)::mode in
  set_modes mode default

let render win (s:State.t) v = match v.mode with
  | GenericScreen {render_fn} -> render_fn win s

let handle_event event v = match v.mode with
  | GenericScreen _ when Event.is_left_click event || Event.key_modal_dismiss event ->
      next_mode v
  | GenericScreen _ -> `None, v

let handle_tick win time v = match v.mode with
  | GenericScreen _ -> v
