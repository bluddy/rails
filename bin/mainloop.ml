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

  let fps = 15l in (* WSL has limited fps support *)
  let render_wait_time = Int32.(1000l/fps) in
  let tick_fps = 15l in (* How fast we want to update state *)
  let tick_wait_time = Int32.(1000l/tick_fps) in
  let sleep_time = 30l in (* less than render_wait_time *)

  let data, v = init_fn win in

  let time = Sdl.get_ticks () in
  let last_render_time = ref time in
  let last_tick_time = ref time in

  let rec update_loop data  =
    let rec event_loop data =
      let event =
        (* convert to our Event.t *)
        if Sdl.poll_event some_event then Event.of_sdl event ~zoom
        else Event.NoEvent
      in
      match event with
      | Quit -> data, `Quit
      | NoEvent -> data, `NoEvent
      | EventNotRelevant ->
          (* Get rid of events we don't care about *)
          event_loop data
      | _ -> 
        let time = Sdl.get_ticks () in
        let data =
          if Int32.(time - !last_tick_time > tick_wait_time) then (
            last_tick_time := time;
            v.handle_tick data time
          ) else
            data
        in
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
      let tick_diff = time - !last_tick_time in
      let data =
        if tick_diff >= tick_wait_time then (
          last_tick_time := time;
          v.handle_tick data time
        )
        else data
      in
      let render_diff = time - !last_render_time in
      if render_diff >= render_wait_time then (
        last_render_time := time;
        v.render data;
        Sdl.render_present win.renderer;
      );
      if render_wait_time - render_diff >= sleep_time && tick_wait_time - tick_diff >= sleep_time then (
        (* nap *)
        Sdl.delay sleep_time;
      );
      update_loop data
  in
  update_loop data;

  Sdl.destroy_renderer win.renderer;
  Sdl.destroy_window win.window;
  Sdl.quit ();
  exit 0


