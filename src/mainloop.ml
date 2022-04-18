open Containers
open Tsdl

module R = Renderer

type 'a t = {
  update: 'a -> Event.t -> 'a * bool;
  render: 'a -> 'a;
}

let main init_fn =
  let win = R.create 320 200 ~zoom:2. in
  let event = Sdl.Event.create () in
  let some_event = Some event in (* For reducing allocation with SDL *)

  let data, v = init_fn win in

  let rec event_loop data (last_time:int32) =
    let has_event = Sdl.poll_event some_event in
    let event =
      if has_event then Event.of_sdl event else Event.NoEvent
    in
    let render_wait_time = 30l in
    match event with
    | Quit -> Result.return ()
    | _ -> 
      let data, stop = v.update data event in
      let data = v.render data in

      let open Int32.Infix in
      let time = Sdl.get_ticks () in
      if time - last_time < render_wait_time then
        Sdl.delay (render_wait_time - time + last_time);

      Sdl.render_present win.renderer;
      if stop then
        Result.return ()
      else
        event_loop data time
  in
  ignore(event_loop data @@ Sdl.get_ticks ());

  Sdl.destroy_renderer win.renderer;
  Sdl.destroy_window win.window;
  Sdl.quit ();
  exit 0


