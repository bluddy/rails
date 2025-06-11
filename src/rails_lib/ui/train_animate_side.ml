open Containers
open Train_animate_side_d

module R = Renderer

let fps = 15
let wait_time = 1000/fps

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
    anim_idx=0;
    smoke_idx=0;
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

let render win (s:State.t) v =
  Station_report.render win s v.station ~show_demand:false;
  match v.rail with
  | `Back ->
      let y = 186 in
      let engine_tex = Hashtbl.find s.textures.small_engine v.engine in
      let h = R.Texture.get_h engine_tex in
      R.Texture.render win ~x:v.x ~y:(y-h) engine_tex

  | `Front ->
      let y = 193 in
      let engine = Hashtbl.find s.textures.engine_anim v.engine in
      let engine_h = R.Texture.get_h engine.tex in
      R.Texture.render win ~x:v.x ~y:(y-engine_h) engine.tex;
      let len = Array.length engine.anim in

      if not v.paused then begin
        (* Draw animating wheels *)
        if len > 0 then
          R.Texture.render win ~x:(v.x+engine.anim_x) ~y:(y-engine_h+engine.anim_y) engine.anim.(v.anim_idx);

        (* Draw smoke *)
        begin match engine.smoke_x with
        | None -> ()
        | Some smoke_x ->
            let smoke_arr = Hashtbl.find s.textures.smoke `SmokeSideBig in
            let smoke_tex = smoke_arr.(v.smoke_idx) in
            let smoke_h = R.Texture.get_h smoke_tex in
            R.Texture.render win ~x:(v.x + smoke_x - smoke_x_off) ~y:(y - engine_h - smoke_h) smoke_tex
        end;
      end;

      (* Draw cars *)
      let _ =
        List.fold_left (fun x car ->
          let car_tex_old, _ = Hashtbl.find s.textures.car_anim car in
          let car_w, car_h = R.Texture.get_w car_tex_old, R.Texture.get_h car_tex_old in
          let x = x - car_w in
          R.Texture.render win ~x ~y:(y - car_h) car_tex_old;
          x
        )
        v.x
        v.cars
      in
      ()

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

        if not v.paused then begin
          (* Run wheel animation if present *)
          let anim_len = Array.length engine.anim in
          if anim_len > 0 then
            v.anim_idx <- (v.anim_idx + 1) mod anim_len;

          (* Run smoke animation if present *)
          begin match engine.smoke_x with
          | Some _ ->
            let smoke_arr = Hashtbl.find s.textures.smoke `SmokeSideBig in
            let smoke_len = Array.length smoke_arr in
            v.smoke_idx <- (v.smoke_idx + 1) mod smoke_len
          | None -> ()
          end;

          v.x <- v.x + 1;
        end;
        v
  )


