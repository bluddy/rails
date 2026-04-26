open! Containers

module R = Engine.Renderer
module Transition = Engine.Transition
module Pani_render = Engine.Pani_render
module Event = Engine.Event
module Sound = Engine.Sound
module Ega = Engine.Ega
module C = Constants.Intro

open Utils.Infix

type mode =
  | Animation of Engine.Pani_render.t
  | TransitionScreen of Engine.Transition.t
  | GenericScreen of {render_fn: Engine.Renderer.window -> unit; timeout: int option}

type t = {
  mode: mode;
  next_modes: mode list;
}

let render_screen tex win =
  R.clear_screen win;
  R.Texture.render win ~x:0 ~y:0 tex

let make (s:Services.t) =
  let make_render_fn tex_name =
      let tex = Hashtbl.find s.textures.images tex_name in
      fun win -> render_screen tex win
  in
  let _add_transition tex1 tex2 =
    let old_render_fn = match tex1 with
    | Some tex1 -> make_render_fn tex1
    | None -> R.clear_screen
    in
    let wait_time = if Option.is_some tex1 then C.wait_time else 0 in
    let render_fn = make_render_fn tex2 in
    let trans = Transition.make s.win s.random ~wait_time ~old_render_fn ~render_fn in
    TransitionScreen trans in
  let modes = [] in
  let modes = (GenericScreen {render_fn=make_render_fn `MPS_labs; timeout=Some 0})::modes in
  let modes =
    let file = "data/covert/TITLE2.PAN" in
    Animation(Sound.pani_create s.sound file)::modes in
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
  | _::_ -> set_modes v.next_modes v, `Stay
  | [] -> v, `Exit

let render win v = match v.mode with
  | TransitionScreen t -> Transition.render win t
  | Animation state ->
      (* Hack for blue background *)
      R.clear_screen win;
      R.draw_rect win ~x:0 ~y:0 ~w:320 ~h:200 ~color:Ega.blue ~fill:true;
      Pani_render.render ~clear_screen:false win state
  | GenericScreen {render_fn; _} -> render_fn win

let handle_event event v = match v.mode with
  | GenericScreen _ when Event.modal_dismiss event -> set_next_mode v
  | GenericScreen _ -> v, `Stay
  | Animation state ->
      begin match Pani_render.handle_event event state with
      | state', `Stay when state =!= state' -> {v with mode=Animation state'}, `Stay
      | _, `Stay -> v, `Stay
      | _, `Exit -> set_next_mode v
      end
  | TransitionScreen state ->
      begin match Transition.handle_event event state with
      | `Stay -> v, `Stay
      | `Exit -> set_next_mode v
      end

let handle_tick time v = match v.mode with
  | Animation state ->
      let state2, quit = Pani_render.handle_tick time state in
      if state2 === state then v, quit else {v with mode=Animation state2}, quit
  | TransitionScreen state ->
      let status, state2 = Transition.handle_tick time state in
      begin match status with
      | `Stay when state2 === state -> v, `Stay
      | `Stay -> {v with mode=TransitionScreen state2}, `Stay
      | `Exit -> set_next_mode v
      end
  | GenericScreen ({timeout=Some 0;_} as state) -> {v with mode=GenericScreen{state with timeout=Some(time + C.wait_time * 1000)}}, `Stay
  | GenericScreen {timeout=Some end_time;_} when time >= end_time -> set_next_mode v
  | GenericScreen _ -> v, `Stay


