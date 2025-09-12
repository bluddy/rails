open Containers
open Tsdl

module R = Renderer

type 'a t = {
  handle_event: 'a -> Event.t -> int -> 'a * bool;
  handle_tick: 'a -> int -> 'a;
  render: 'a -> unit;
}

let main init_fn =
  let zoom_x, zoom_y = 3., 3. *. 1.2  in
  let win = R.create 320 200 ~zoom_x ~zoom_y in
  let event = Sdl.Event.create () in
  let some_event = Some event in (* For reducing allocation with SDL *)

  let fps = 30 in (* WSL has limited fps support *)
  let render_wait_time = 1000/fps in
  let tick_fps = 15 in (* How fast we want to update state *)
  let tick_wait_time = 1000/tick_fps in
  let sleep_time = 30 in (* less than render_wait_time *)
  let sleep_time_l = sleep_time |> Int32.of_int in

  let data, v = init_fn win in

  let time = Utils.get_time () in
  let last_render_time = ref time in
  let last_tick_time = ref time in

  let rec update_loop data  =
    let rec event_loop data =
      let event =
        (* convert to our Event.t *)
        if Sdl.poll_event some_event then Event.of_sdl event ~zoom_x ~zoom_y
        else Event.NoEvent
      in
      match event with
      | Quit -> data, `Quit
      | NoEvent -> data, `NoEvent
      | EventNotRelevant ->
          (* Get rid of events we don't care about *)
          event_loop data
      | _ -> 
        let time = Sdl.get_ticks () |> Int32.to_int in
        let data =
          if time - !last_tick_time > tick_wait_time then (
            last_tick_time := time;
            v.handle_tick data time
          ) else
            data
        in
        let time = Sdl.get_ticks () |> Int32.to_int in
        let data, quit = v.handle_event data event time in
        if quit then data, `Quit
        else
          event_loop data
    in
    (* first handle all events *)
    let data, response = event_loop data in
    match response with
    | `Quit -> ()
    | _ ->
      let time = Sdl.get_ticks () |> Int32.to_int in
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
        Sdl.delay sleep_time_l;
      );
      update_loop data
  in
  update_loop data;

  Sdl.destroy_renderer win.renderer;
  Sdl.destroy_window win.window;
  Sdl.quit ();
  exit 0


