open! Containers

module R = Renderer
module B = Backend
module C = Constants
module M = Money

open Utils.Infix

include Intro_d

let render_screen tex win =
  R.clear_screen win;
  R.Texture.render win ~x:0 ~y:0 tex

let clear_screen win =
  R.clear_screen win

let create (s:State.t) =
  let empty_screen =
    GenericScreen{render_fn=clear_screen; transition=None; end_transition=false}
  in
  let add_screen tex mode =
    let tex = Hashtbl.find s.textures.misc tex in
    let screen = GenericScreen{render_fn=render_screen tex; transition=None; end_transition=true}
    in
    screen::mode in
  let modes = [empty_screen] in
  let modes = add_screen `LogoMicroprose modes in
  let modes = add_screen `LogoMPS modes in
  let modes = add_screen `Credits modes in
  let modes =
    let file = "TITLEM.PAN" in
    Animation(Pani_render.create file)::modes in
  match List.rev modes with
  | x::xs -> {mode=x; next_modes=xs}
  | _ -> assert false

let set_modes l v = match l with
  | x::xs -> {mode=x; next_modes=xs}
  | _ -> v

let next_mode v = match v.next_modes with
  | x::_ -> Some x
  | _ -> None

let set_next_mode v = match v.next_modes with
  | _::_ -> `None, set_modes v.next_modes v
  | [] -> `Exit, v

let rec render ?(freeze=false) win (s:State.t) v = match v.mode with
  | GenericScreen {render_fn; transition=Some t; _} when not freeze ->
      render_fn win;
      Transition.render win t

  | GenericScreen {render_fn; transition=None; end_transition=true} when not freeze && not @@ List.is_empty v.next_modes ->
    (* Have to create it here because we have win *)
    let render_fn =
      let next_mode = List.hd v.next_modes in
      let next_v = {v with mode=next_mode} in
      fun win -> render win ~freeze:true s next_v in
    let tr = Transition.make win s.random render_fn in
    render_fn win

  | GenericScreen {render_fn; transition=None;_} -> render_fn win

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

