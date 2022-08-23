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
      let engine = Hashtbl.find s.textures.engine_anim v.engine in
      R.Texture.render win ~x:v.x ~y:v.y engine.tex;
      let len = Array.length engine.anim in
      (* Draw animating wheels *)
      if len > 0 then
        R.Texture.render win ~x:(v.x+engine.anim_x) ~y:(v.y+engine.anim_y) engine.anim.(v.anim_idx);
      (* Draw smoke *)
      begin match engine.smoke_x with
      | None -> ()
      | Some smoke_x ->
          let smoke_arr = Hashtbl.find s.textures.smoke `SmokeSideBig in
          let smoke_tex = smoke_arr.(v.smoke_idx) in
          let w, h = R.Texture.get_w smoke_tex, R.Texture.get_h smoke_tex in
          R.Texture.render win ~x:(v.x + smoke_x - w) ~y:(v.y - h) smoke_tex
      end

let handle_tick (s:State.t) v time =
  if time - v.last_time < wait_time then v
  else (
    v.last_time <- time;
    match v.rail with
    | `Back ->
        let engine_tex = Hashtbl.find s.textures.small_engine v.engine in
        let width = R.Texture.get_w engine_tex in
        v.x <- v.x - 1;
        if v.x + width < 0 then (
          let engine_tex = Hashtbl.find s.textures.engine_anim v.engine in
          let x = 0 - engine_tex.Textures.TrainAnim.w in
          {v with rail=`Front; x}
        ) else
          v

    | `Front ->
        let engine = Hashtbl.find s.textures.engine_anim v.engine in
        let anim_len = Array.length engine.anim in
        if anim_len > 0 then
          v.anim_idx <- (v.anim_idx + 1) mod anim_len;

        begin match engine.smoke_x with
        | Some _ ->
          let smoke_arr = Hashtbl.find s.textures.smoke `SmokeSideBig in
          let smoke_len = Array.length smoke_arr in
          v.smoke_idx <- (v.smoke_idx + 1) mod smoke_len
        | None -> ()
        end;

        v.x <- v.x + 1;
        v
  )


