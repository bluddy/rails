open! Containers

module R = Renderer
module B = Backend
module C = Constants
module M = Money

open Utils.Infix

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
  let mode = add_screen `Credits mode in
  let mode =
    let file = "intro.pan" in
    Animation(Pani_render.create file)::mode in
  set_modes (List.rev mode) default

let render win (s:State.t) v = match v.mode with
  | GenericScreen {render_fn} -> render_fn win s
  | Animation state -> Pani_render.render win state

let handle_event event v = match v.mode with
  | Animation _
  | GenericScreen _ when Event.is_left_click event || Event.key_modal_dismiss event ->
      next_mode v
  | _ -> `None, v

let handle_tick time v = match v.mode with
  | Animation state ->
      let state2 = Pani_render.handle_tick time state in
      if state2 === state then v else {v with mode=Animation state2}
  | _ -> v

