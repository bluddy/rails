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
  | DoubleAnimation of Engine.Pani_render.t * Engine.Pani_render.t
  | SingleAnimation of Engine.Pani_render.t
  | Picture of {render_fn: Engine.Renderer.window -> unit; timeout: int option}

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
  let modes = [] in
  let modes = (Picture {render_fn=make_render_fn `MPS_labs; timeout=Some 0})::modes in
  let title_pani = Sound.pani_create s.sound "data/covert/TITLE2.PAN" in
  let modes = SingleAnimation(title_pani)::modes in
  let modes = DoubleAnimation(title_pani, Sound.pani_create s.sound "data/covert/CREDITS.PAN")::modes in
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
  | SingleAnimation state ->
      (* Hack for blue background *)
      R.clear_screen win;
      R.draw_rect win ~x:0 ~y:0 ~w:320 ~h:200 ~color:Ega.blue ~fill:true;
      Pani_render.render ~clear_screen:false win state
  | DoubleAnimation(s1, s2) ->
      R.clear_screen win;
      R.draw_rect win ~x:0 ~y:0 ~w:320 ~h:200 ~color:Ega.blue ~fill:true;
      Pani_render.render ~clear_screen:false win s1;
      Pani_render.render ~clear_screen:false win s2;
  | Picture {render_fn; _} -> render_fn win

let handle_event event v =
  let handle_state state fn =
    begin match Pani_render.handle_event event state with
    | state', `Stay when state =!= state' -> {v with mode=fn state'}, `Stay
    | _, `Stay -> v, `Stay
    | _, `Exit -> set_next_mode v
    end
  in
  match v.mode with
  | Picture _ when Event.modal_dismiss event -> set_next_mode v
  | Picture _ -> v, `Stay
  | SingleAnimation state ->
      handle_state state @@ fun state -> SingleAnimation state
  | DoubleAnimation(s1, s2) ->
      handle_state s2 @@ fun state -> DoubleAnimation(s1, state)

let handle_tick time v =
  let state_change state fn =
    let state2, quit = Pani_render.handle_tick time state in
    if state2 === state then v, quit else {v with mode=fn state2}, quit
  in
  match v.mode with
  | SingleAnimation state -> state_change state @@ fun state -> SingleAnimation state
  | DoubleAnimation(s1, s2) -> state_change s2 @@ fun state -> DoubleAnimation(s1, state)
  | Picture ({timeout=Some 0;_} as state) -> {v with mode=Picture{state with timeout=Some(time + C.wait_time * 1000)}}, `Stay
  | Picture {timeout=Some end_time;_} when time >= end_time -> set_next_mode v
  | Picture _ -> v, `Stay


