open! Containers

module R = Renderer
module B = Backend
module C = Constants.Intro
module M = Money

open Utils.Infix

include Intro_d

let render_screen tex win =
  R.clear_screen win;
  R.Texture.render win ~x:0 ~y:0 tex

let clear_screen win =
  R.clear_screen win

let make (s:State.t) =
  let make_render_fn tex_name =
      let tex = Hashtbl.find s.textures.misc tex_name in
      fun win -> render_screen tex win
  in
  let add_transition tex1 tex2 =
    let old_render_fn = match tex1 with
    | Some tex1 -> make_render_fn tex1
    | None -> clear_screen
    in
    let wait_time = if Option.is_some tex1 then C.wait_time else 0 in
    let render_fn = make_render_fn tex2 in
    let trans = Transition.make s.win s.random ~wait_time ~old_render_fn ~render_fn in
    TransitionScreen trans in
  let modes = (add_transition None `LogoMicroprose)::[] in
  let modes = (add_transition (Some `LogoMicroprose) `LogoMPS)::modes in
  let modes = (add_transition (Some `LogoMPS) `Credits)::modes in
  let modes = (GenericScreen {render_fn=make_render_fn `Credits; timeout=Some 0})::modes in
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
  | _::_ -> `Stay, set_modes v.next_modes v
  | [] -> `Exit, v

let render win v = match v.mode with
  | TransitionScreen t -> Transition.render win t
  | Animation state -> Pani_render.render win state
  | GenericScreen {render_fn; _} -> render_fn win

let handle_event event v = match v.mode with
  | (GenericScreen _ | Animation _) when Event.is_left_click event || Event.key_modal_dismiss event -> set_next_mode v
  | GenericScreen _ | Animation _ -> `Stay, v
  | TransitionScreen state ->
      begin match Transition.handle_event event state with
      | `Stay -> `Stay, v
      | `Exit -> set_next_mode v
      end

let handle_tick time v = match v.mode with
  | Animation state ->
      let state2 = Pani_render.handle_tick time state in
      if state2 === state then `Stay, v else `Stay, {v with mode=Animation state2}
  | TransitionScreen state ->
      let status, state2 = Transition.handle_tick time state in
      begin match status with
      | `Stay when state2 === state -> `Stay, v
      | `Stay -> `Stay, {v with mode=TransitionScreen state2}
      | `Exit -> set_next_mode v
      end
  | GenericScreen ({timeout=Some 0;_} as state) -> `Stay, {v with mode=GenericScreen{state with timeout=Some(time + C.wait_time * 1000)}}
  | GenericScreen {timeout=Some end_time;_} when time >= end_time -> set_next_mode v
  | GenericScreen _ -> `Stay, v


