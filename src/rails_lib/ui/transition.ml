open Containers
module R = Renderer
module C = Constants.Transition

type t = {
  old_render_fn: (R.window -> unit) option;
  transition: R.Transition.t;
  mutable last_tick: int;
  finished: bool;
}

let make win random ?old_render_fn render_fn =
  let transition = R.Transition.make win random in
  (* Set the final image *)
  R.Transition.render_offscreen win render_fn transition;
  R.Transition.clear transition;
  {old_render_fn; transition; last_tick=0; finished=false}

let render win v =
  match v.old_render_fn with
  | Some render_fn when not v.finished -> render_fn win
  | _ -> ();
  R.Transition.render win v.transition

let handle_tick v time =
  (* Inner transition is entirely mutable *)
  if v.finished then `Exit, v else
  let new_time = v.last_tick + C.tick_delta in
  if time >= new_time then (
    v.last_tick <- time;
    let status = R.Transition.step C.step_pixels v.transition in
    match status with
    | `Done -> `Stay, {v with finished=true}
    | _ -> `Stay, v
  ) else `Stay, v

let handle_event (event:Event.t) v =
  if not v.finished then `Stay else
  match event with
  | Key {down=true; _} -> `Exit
  | MouseButton {down=true; _} -> `Exit
  | _ -> `Stay


