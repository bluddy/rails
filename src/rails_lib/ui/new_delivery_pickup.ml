open Containers
open Utils.Infix

(* UI for 1st delivery/pickup of a good notice *)

module R = Renderer
module B = Backend
module C = Constants

include New_delivery_pickup_d

let is_delivery v = Option.is_some v.delivery

(* Create the animation that will be used when we add cars *)
let init ?delivery (s:State.t) good loc engine cars =
  let delivery = delivery |> Option.map (fun (src, revenue, amount) -> {src; revenue; amount}) in
  let is_delivery = Option.is_some delivery in
  let x = if is_delivery then 150 else 208 in
  let anim =
    Train_animate_side.init s ~x ~engine ~cars ~paused:false ~station:loc ~rail:`Front
  in
  {
    anim;
    finished=not is_delivery;
    good;
    loc;
    delivery;
  }

let nobaction = Backend.Action.NoAction

let handle_event v event =
  if (Event.pressed_esc event || Event.is_left_click event) then
    if v.finished then v, `Exit
    else
      {v with finished=true}, `Stay
  else
    v, `Stay

let handle_tick s time v =
  let anim = 
    if v.finished then v.anim
    else
      Train_animate_side.handle_tick s v.anim time
  in
  let finished =
    let x_finish = if is_delivery v then 280 else 208 in
    v.finished || Train_animate_side.get_x v.anim >= x_finish
  in
  [%up {v with anim; finished}]

let render win (s:State.t) v =
  Train_animate_side.render win s v.anim;
  if v.finished then (
  )

