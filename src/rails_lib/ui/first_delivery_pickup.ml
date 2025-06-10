open Containers
open Utils.Infix

module R = Renderer
module B = Backend
module C = Constants

type delivery = {
  src: Utils.loc;
  loc: Utils.loc;
  amount: int;
  revenue: Money.t;
}

type t = {
  anim: Train_animate_side_d.t;
  finished: bool;
  good: Goods.t;
  delivery: delivery option;
}

let is_delivery v = Option.is_some v.delivery

(* Create the animation that will be used when we add cars *)
let init (s:State.t) ~engine =
  (* Find station with engine shop *)
  let station =
    try
      Station_map.filter 
      (fun station -> Station.can_build_train station)
      s.backend.stations
      |> Iter.head_exn
    with Invalid_argument _ -> invalid_arg "No station with engine found"
  in
  let anim =
    let engine = engine.Engine.make in
    Train_animate_side.init s ~engine ~cars:[] ~paused:false ~station:station.loc ~rail:`Back
  in
  {
    anim;
    finished=false;
  }

let nobaction = Backend.Action.NoAction

let handle_event (s:State.t) v event =
  if v.train_done && (Event.pressed_esc event || Event.is_left_click event) then
    let station = v.anim.station in
    let other_station =
      Track_graph.connected_stations_dirs s.backend.graph s.backend.track [station]
      |> Utils.LocuHSet.to_iter |> Iter.head |> Option.map fst
    in
    v, Backend.Action.BuildTrain{engine=v.anim.engine;
                                 cars=v.anim.cars;
                                 station;
                                 other_station;
                                 player_idx=C.player}
  else
    v, nobaction

let handle_tick s v time =
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
  ()

let is_done v = v.finished

let nobaction = Backend.Action.NoAction

let handle_event (s:State.t) v (event:Event.t) = match v with
  | `ChooseEngine ->
      let engine_opt =
        Choose_engine.handle_event event s.backend.engines ~year:(B.get_year s.backend)
      in
      begin match engine_opt with
      | Some engine ->
          let state = AddCars.init s ~engine in
          `AddCars state, nobaction
      | None ->
          `ChooseEngine, nobaction
      end
  | `AddCars state ->
      let state2, action = AddCars.handle_event s state event in
      if state =!= state2 then
        `AddCars state2, action
      else
        v, action

let render win (s:State.t) v = match v with
  | `ChooseEngine ->
        Choose_engine.render win s ~engines:s.backend.engines ~year:(B.get_year s.backend)
  | `AddCars state ->
        AddCars.render win s state 

