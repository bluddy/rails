open Containers
module R = Renderer
module C = Constants.Transition

let src = Logs.Src.create "transition" ~doc:"Transition"
module Log = (val Logs.src_log src: Logs.LOG)

type state =
  | Static of {until: int option}
  | Animating of {mutable last_tick: int}
  | Done
  [@@deriving show]

type t = {
  old_render_fn: (R.window -> unit);
  transition: R.Transition.t;  (* also stores new render_fn *)
  wait_time: int; (* in secs *)
  state: state;
}

let make win random ~wait_time ~old_render_fn ~render_fn =
  let transition = R.Transition.make win random in
  (* Set the final image *)
  R.Transition.render_offscreen win render_fn transition;
  R.Transition.clear transition;
  let state = Static {until=None} in
  {old_render_fn; transition; state; wait_time}

let render win v = match v.state with
  | Static _ -> v.old_render_fn win
  | Animating _ ->
     v.old_render_fn win;
     R.Transition.render win v.transition
  | Done ->
     R.Transition.render win v.transition

let is_finished v = match v.state with Done -> true | _ -> false

let handle_tick time v =
  (* Inner transition is entirely mutable *)
  (* Log.info (fun f -> f "time is %d, state is %s" time @@ show_state v.state); *)
  match v.state with
  | Done -> `Exit, v
  | Static {until=None} -> `Stay, {v with state=Static{until=Some (time + v.wait_time * 1000)}}
  | Static {until=Some end_time} when end_time <= time -> `Stay, {v with state=Animating{last_tick=0}}
  | Static {until=Some _} -> `Stay, v
  | Animating ({last_tick} as a) ->
    let new_time = last_tick + C.tick_delta in
    if time >= new_time then (
      a.last_tick <- time;
      let status = R.Transition.step C.step_pixels v.transition in
      match status with
      | `Done -> `Stay, {v with state=Done}
      | _ -> `Stay, v
    ) else `Stay, v

let handle_event (event:Event.t) v =
  if not @@ is_finished v then `Stay else
  match event with
  | Key {down=true; _} -> `Exit
  | MouseButton {down=true; _} -> `Exit
  | _ -> `Stay


