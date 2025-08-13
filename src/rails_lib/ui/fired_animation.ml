open! Containers
module B = Backend
module R = Renderer

include Fired_animation_d

let fps = 15
let wait_time = 1000/fps

let sp = Printf.sprintf

let next_frame = function
  | `Escape1 -> `Escape2
  | `Escape2 -> `Escape1

let make (s:State.t) ~fired_by player_ctr =
  let b = s.backend in
  let player = B.get_player player_ctr b in
  let station_loc = Player.get_first_station player in
  let text =
    sp "%s president leaves" @@ B.get_handle player_ctr b,
    "town after meeting",
    sp "with %s."
      @@ match fired_by with `Management -> "new managements" | `Stockholders -> "stockholders"
  in
  let newspaper = Newspaper.make_fancy s text b.params in 
  {
    last_time=0;
    ctr=0;
    station_loc;
    newspaper;
  }

let render win (s:State.t) v =
  Option.iter (fun loc ->
    Station_report.render win s loc ~show_demand:false;
    let frame = (v.ctr / 4) mod 2 in
    let frame = match frame with 0 -> `Escape1 | 1 -> `Escape2 | _ -> assert false in
    let tex = Hashtbl.find s.textures.misc (frame :> Textures.Misc.t) in
    let _draw_sign =
      Menu.MsgBox.render_box ~color:Ega.blue win 138 150 44 14;
      let text = "under new\nmanagement" in
      Fonts.Render.write win s.fonts ~color:Ega.white ~idx:`Tiny text ~x:141 ~y:153;
    in
    R.Texture.render win ~x:(v.ctr + 160) ~y:212 tex
  ) v.station_loc;
  Newspaper.render win s v.newspaper

let handle_tick v time =
  if time - v.last_time < wait_time || v.ctr > 320 then v
  else (
    v.last_time <- time;
    v.ctr <- v.ctr + 1;
    v
  )

