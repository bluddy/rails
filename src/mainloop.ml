open Containers
open Tsdl

module R = Renderer

type 'a t = {
  handle_event: 'a -> Event.t -> 'a * bool;
  handle_tick: 'a -> int32 -> 'a;
  render: 'a -> unit;
}

let main init_fn =
  let zoom = 4. in
  let win = R.create 320 200 ~zoom in
  let event = Sdl.Event.create () in
  let some_event = Some event in (* For reducing allocation with SDL *)

  let data, v = init_fn win in
  let render_wait_time = 30l in
  let sleep_time = 10l in

  let rec update_loop data (last_time:int32) =
    let rec event_loop data =
      let event =
        (* convert to our Event.t *)
        if Sdl.poll_event some_event then Event.of_sdl event ~zoom
        else Event.NoEvent
      in
      match event with
      | Quit -> data, `Quit
      | NoEvent -> data, `NoEvent
      | _ -> 
        let data, quit = v.handle_event data event in
        if quit then data, `Quit
        else
          event_loop data
    in
    (* first handle all events *)
    let data, response = event_loop data in
    match response with
    | `Quit -> ()
    | _ ->
      let open Int32.Infix in
      let time = Sdl.get_ticks () in
      let diff = time - last_time in
      if diff >= render_wait_time then (
        let data = v.handle_tick data time in
        v.render data;
        Sdl.render_present win.renderer;
        update_loop data time
      )
      else if render_wait_time - diff < sleep_time then (
        (* no time to nap *)
        update_loop data last_time
      ) else (
        (* time to nap *)
        Sdl.delay sleep_time;
        update_loop data last_time
      )
  in
  update_loop data (Sdl.get_ticks ());

  Sdl.destroy_renderer win.renderer;
  Sdl.destroy_window win.window;
  Sdl.quit ();
  exit 0


