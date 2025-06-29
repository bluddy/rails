open Containers

(* UI for 1st delivery/pickup of a good notice *)

module R = Renderer
module B = Backend
module C = Constants

include New_delivery_pickup_d

let is_delivery v = match v.kind with `Delivery _ -> true | _ -> false

(* Create the animation that will be used when we add cars *)
let init ?pickup ?delivery (s:State.t) good loc engine cars =
  let kind, x, finished = match pickup, delivery with
    | Some buying, _ -> `Pickup {buying}, 208, true
    | _, Some (src, revenue, amount, speed) ->  `Delivery {src; revenue; amount; speed}, 150, false
    | _ -> assert false
  in
  let anim =
    Train_animate_side.init s ~x ~engine ~cars ~paused:false ~station:loc ~rail:`Front
  in
  {
    anim;
    finished;
    good;
    loc;
    kind;
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


let station_name loc s = (B.get_station loc s.State.backend |> Option.get_exn_or "oops "|> Station.get_name)

let render win (s:State.t) v =
  Train_animate_side.render ~show_name:false win s v.anim;
  let fonts = s.fonts in
  let color = Ega.white in
  let write_lg = Fonts.Render.write_shadow win fonts ~color ~idx:`Large in
  let write_sm = Fonts.Render.write_shadow win fonts ~color ~idx:`Standard in
  let station_s = B.get_station v.loc s.backend |> Option.get_exn_or "oops "|> Station.get_name in
  if v.finished then (
    let good_s = Goods.show v.good in
    match v.kind with
    | `Delivery d ->
      let text = Printf.sprintf "First\n%s\ndelivery!" good_s in
      write_lg text ~x:80 ~y:8;
      let text = Printf.sprintf
        "%d %s%s\n\
         picked up in %s\n\
         delivered to %s\n\
         Distance: %d miles.\n\
         Speed: %d miles per hour.\n\
         \n\
         Revenue"
        d.amount (Goods.group_of v.good) good_s
        (B.get_station d.src s.backend |> Option.get_exn_or "oops "|> Station.get_name)
        station_s
        (Utils.classic_dist v.loc d.src)
        d.speed
      in
      write_sm text ~x:80 ~y:63;
      let text = Money.print d.revenue in
      write_lg text ~x:128 ~y:108

    | `Pickup p ->
      let text = Printf.sprintf "%s\nservice\ninaugurated!" good_s in
      write_lg  text ~x:80 ~y:8;
      let text = Printf.sprintf "%s picked up from %s" good_s station_s in
      write_sm text ~x:40 ~y:64;
      let text = Printf.sprintf "%s may be delivered to" good_s in
      write_sm text ~x:40 ~y:82;
      let text = List.map (fun loc -> station_name loc s) p.buying |> String.concat "\n" in
      write_sm text ~x:80 ~y:90;
  )

