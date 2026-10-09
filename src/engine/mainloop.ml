open Containers
open Tsdl

module R = Renderer

type 'a t = {
  handle_event: 'a -> Event.t -> int -> 'a * [`Exit | `Stay];
  handle_tick: 'a -> int -> 'a * [`Exit | `Stay];
  render: 'a -> unit;
}

let main ?zoom ?(adjust_ar=false) ?(shader=R.No_shader) init_fn =
  let win = R.create 320 200 ?zoom ~adjust_ar ~shader in
  let event = Sdl.Event.create () in
  let some_event = Some event in (* For reducing allocation with SDL *)

  let fps = 30 in (* WSL has limited fps support *)
  let render_wait_time = 1000/fps in
  let tick_fps = 20 in (* How fast we want to update state *)
  let tick_wait_time = 1000/tick_fps in
  let sleep_time = 30 in (* less than render_wait_time *)
  let sleep_time_l = sleep_time |> Int32.of_int in

  let data, v = init_fn win in

  let time = Utils.get_time () in
  let last_render_time = ref time in
  let last_tick_time = ref time in

  let rec update_loop data  =
    let rec event_loop data =
      let polled = Sdl.poll_event some_event in
      (* Keep window metrics in sync after fullscreen toggles / resizes *)
      if polled then begin
        match Sdl.Event.(enum (get event typ)) with
        | `Window_event
          when Sdl.Event.(get event window_event_id)
               = Sdl.Event.window_event_size_changed ->
            R.sync_window_size win
        | _ -> ()
      end;
      let event =
        (* convert to our Event.t *)
        if polled then Event.of_sdl event ~zoom_x:win.zoom_x ~zoom_y:win.zoom_y
        else Event.NoEvent
      in
      match event with
      | Quit -> data, `Exit
      | NoEvent -> data, `Stay
      | EventNotRelevant ->
          (* Get rid of events we don't care about *)
          event_loop data
      | Key {down=true; repeat=0; key=Enter; modifiers}
        when Event.Modifiers.mem modifiers `Alt ->
          (* Alt+Enter: toggle fullscreen (window-level concern, handled
             before the game sees the event) *)
          R.toggle_fullscreen win;
          event_loop data
      | _ ->
        let time = Sdl.get_ticks () |> Int32.to_int in
        let data, quit =
          if time - !last_tick_time > tick_wait_time then (
            last_tick_time := time;
            v.handle_tick data time
          ) else
            data, `Stay
        in
        match quit with `Exit -> data, `Exit | _ ->
        let time = Sdl.get_ticks () |> Int32.to_int in
        let data, quit = v.handle_event data event time in
        match quit with `Exit -> data, `Exit
        | _ -> event_loop data
    in
    (* first handle all events *)
    let data, response = event_loop data in
    match response with
    | `Exit -> ()
    | _ ->
      let time = Sdl.get_ticks () |> Int32.to_int in
      let tick_diff = time - !last_tick_time in
      let data, quit =
        if tick_diff >= tick_wait_time then (
          last_tick_time := time;
          v.handle_tick data time
        )
        else data, `Stay
      in
      match quit with `Exit -> () | _ ->
      let render_diff = time - !last_render_time in
      if render_diff >= render_wait_time then (
        last_render_time := time;
        R.render_wrap win v.render data;
      );
      if render_wait_time - render_diff >= sleep_time && tick_wait_time - tick_diff >= sleep_time then (
        (* nap *)
        Sdl.delay sleep_time_l;
      );
      update_loop data
  in
  update_loop data;

  
  Sdl.destroy_window win.window;
  Sdl.quit ();
  exit 0


