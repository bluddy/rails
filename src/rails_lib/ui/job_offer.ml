open! Containers

module R = Renderer
module B = Backend
module M = Money
module C = Constants

let sp = Printf.sprintf

let create job (s:State.t) =
  let fonts = s.fonts in
  let text = sp
    "You are offered a job as\n\
    %s."
    (Jobs.show job)
  in
  let msgbox = Menu.MsgBox.make_basic ~x:154 ~y:8 ~fonts text in
  (job, msgbox)

let render state win (s:State.t) =
  let region = s.backend.params.region in
  let fonts = s.fonts in
  R.paint_screen win ~color:Ega.white;
  R.draw_rect win ~x:2 ~y:2 ~w:(320-4) ~h:(200-4) ~color:Ega.black ~fill:false;

  let msg_job = fst state in
  let _draw_pic =
    let tex = Hashtbl.find s.textures.jobs msg_job in
    let x, y = 314 - tex.w, 195 - tex.h in
    R.Texture.render win ~x ~y tex;
  in

  let x = 8 in
  Jobs.fold region (fun (y:int) job ->
    let job_s = Jobs.show job in
    let color = if Jobs.equal job msg_job then Ega.black else Ega.gray in
    Fonts.Render.write win fonts ~idx:4 ~x ~y ~color job_s;
    y + 8)
  8
  |> ignore;

  let msgbox = snd state in
  Menu.MsgBox.render win s msgbox


