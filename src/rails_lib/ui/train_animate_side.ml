open Containers
open Train_animate_side_d

module R = Renderer

let fps = 15
let wait_time = 1000/fps

(* wheels: temp_cycle /2 mod 3 is offset *)
(* smoke: temp_cycle /3 mod 6 is offset *)

let smoke_x_off = 76 (* depends on smoke image *)

let init ?x (s:State.t) ~engine ~cars ~station ~rail ~paused =
  let engine_tex = Hashtbl.find s.textures.engine_anim engine in
  let x = match x, rail with
    | Some x, `Back -> x
    | _, `Back -> 62
    | Some x, `Front -> x - engine_tex.w
    | _, `Front -> 0 - engine_tex.w
  in
  {
    x;  (* left index of train engine *)
    ctr=0;
    last_time=0;
    rail;
    engine;
    cars;
    paused;
    station;
  }

let pause v = {v with paused=true}
let unpause v = {v with paused=false}

let get_x v = v.x

let draw_engine win (s:State.t) x y engine =
  (* x: left of engine. y: bottom of engine *)
  let engine = Hashtbl.find s.textures.engine_anim engine in
  let engine_h = R.Texture.get_h engine.tex in
  R.Texture.render win ~x ~y:(y-engine_h) engine.tex

let draw_cars win (s:State.t) x y cars =
  (* x: right of cars. y: bottom of cars *)
  List.fold_left (fun x car ->
    let car_tex_old, _ = Hashtbl.find s.textures.car_anim car in
    let car_w, car_h = R.Texture.get_w car_tex_old, R.Texture.get_h car_tex_old in
    let x = x - car_w in
    R.Texture.render win ~x ~y:(y - car_h) car_tex_old;
    x)
  x
  cars

let train_end_at_screen_edge (s:State.t) v =
  match v.rail with
  | `Front ->
    (* Check if at edge *)
    let train_end =
      List.fold_left (fun x car ->
        (* TODO: handle new car texture *)
        let car_tex_old, _car_tex_new = Hashtbl.find s.textures.car_anim car in
        let w = R.Texture.get_w car_tex_old in
        x - w)
      v.x
      v.cars
    in
    train_end = 0
  | `Back -> false

let render ?(show_name=true) win (s:State.t) v =
  Station_report.render win s v.station ~show_demand:false ~show_name;
  match v.rail with
  | `Back ->
      let y = 186 in
      let engine_tex = Hashtbl.find s.textures.small_engine v.engine in
      let h = R.Texture.get_h engine_tex in
      R.Texture.render win ~x:v.x ~y:(y-h) engine_tex

  | `Front ->
      let y = 193 in

      draw_engine win s v.x y v.engine;

      if not v.paused then begin
        let engine = Hashtbl.find s.textures.engine_anim v.engine in
        let engine_h = R.Texture.get_h engine.tex in
        let _draw_wheel_anim =
          let len = Array.length engine.anim in
          if len > 0 then
            let anim_idx = (v.ctr / 2) mod len in
            R.Texture.render win ~x:(v.x+engine.anim_x) ~y:(y-engine_h+engine.anim_y) engine.anim.(anim_idx);
        in
        let _draw_smoke =
          Option.iter (fun smoke_x ->
              let smoke_arr = Hashtbl.find s.textures.smoke `SmokeSideBig in
              let smoke_idx = (v.ctr / 3) mod (Array.length smoke_arr) in
              let smoke_tex = smoke_arr.(smoke_idx) in
              let smoke_h = R.Texture.get_h smoke_tex in
              R.Texture.render win ~x:(v.x + smoke_x - smoke_x_off) ~y:(y - engine_h - smoke_h) smoke_tex
          ) engine.smoke_x;
        in
        ()
      end;

      draw_cars win s v.x y v.cars |> ignore

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
        if not v.paused then (
          v.ctr <- v.ctr + 1;
          v.x <- v.x + 1;
        );
        v
  )


