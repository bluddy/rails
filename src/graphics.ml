open Containers
open Tsdl
module Ndarray = Owl_base_dense_ndarray.Generic

module R = Renderer

type 'a t = {
  init: R.window -> 'a;
  update: 'a -> 'a;
  render: R.window -> 'a -> 'a;
}

let main v =
  let win = R.create 320 200 ~zoom:2. in

  let data = v.init win in

  let event = Sdl.Event.create () in
  let rec event_loop data (last_time:int32) =
    let stop =
      if Sdl.poll_event (Some event) then
        match Sdl.Event.(enum (get event typ)) with
        | `Quit -> true
        | `Key_down ->
            let keycode = Sdl.Event.(get event keyboard_keycode) in
            keycode = Sdl.K.escape
        | _ -> false
      else false
    in
    let render_wait_time = 30l in
    if stop then Result.return () else

      let data = v.update data in
      let data = v.render win data in

      let open Int32.Infix in
      let time = Sdl.get_ticks () in
      if time - last_time < render_wait_time then
        Sdl.delay (render_wait_time - time + last_time);

      Sdl.render_present win.renderer;
      event_loop data time
  in
  ignore(event_loop data @@ Sdl.get_ticks ());

  Sdl.destroy_renderer win.renderer;
  Sdl.destroy_window win.window;
  Sdl.quit ();
  exit 0


