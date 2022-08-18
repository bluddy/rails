open Containers
open Train_animate_side_d

module R = Renderer

let fps = 15
let wait_time = 1000/fps

let init ?(pause_at_cars=false) (s:State.t) ~engine ~cars ~station_x ~station_y ~rail ~moving =
  let x, y = match rail with
    | `Back ->
        let engine_tex = Hashtbl.find s.textures.small_engine engine in
        let h = R.Texture.get_h engine_tex in
        62, 186 - h
    | `Front ->
        let engine_tex = Hashtbl.find s.textures.engine_anim engine in
        0 - engine_tex.w, 193 - engine_tex.h
  in
  {
    x; y;
    anim_idx=0;
    smoke_idx=0;
    last_time=0;
    rail;
    engine;
    cars;
    visible=false;
    pause_at_cars=if pause_at_cars then Some false else None;
    station_x;
    station_y;
    moving;
  }

let render win (s:State.t) v =
  Station_view.render win s v.station_x v.station_y ~show_demand:false;
  match v.rail with
  | `Back ->
      let engine_tex = Hashtbl.find s.textures.small_engine v.engine in
      R.Texture.render win ~x:v.x ~y:v.y engine_tex

  | `Front ->
      let engine_tex = Hashtbl.find s.textures.engine_anim v.engine in
      R.Texture.render win ~x:v.x ~y:v.y engine_tex.tex

let handle_tick (_s:State.t) v time =
  if time - v.last_time < wait_time then v
  else (
    v.last_time <- time;
    (match v.rail with
    | `Back ->
        v.x <- v.x - 1
    | `Front ->
        v.x <- v.x + 1);
    v
  )


