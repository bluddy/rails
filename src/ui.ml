open Containers
open Tsdl
module Ndarray = Owl_base_dense_ndarray.Generic

let sdl_texture_of_ndarray renderer arr =
  let h, w = Ndarray.nth_dim arr 0, Ndarray.nth_dim arr 1 in
  let open Result.Infix in
  Sdl.create_rgb_surface_with_format_from (Bigarray.reshape_1 arr (w*h*3))
    ~w ~h ~depth:24 ~pitch:(w*3) Sdl.Pixel.format_rgb24
  >>= fun surf ->
  Sdl.create_texture_from_surface renderer surf

let make_texture r pic =
  match sdl_texture_of_ndarray r pic with
  | Error(`Msg e) -> Sdl.log "Couldn't create texture from surface: %s" e; exit 1
  | Ok t -> t

let main () =
  let win_w, win_h = 640, 480 in
  let window, r =
    match Sdl.init Sdl.Init.video with
    | Error(`Msg e) -> Sdl.log "Init error: %s" e; exit 1
    | Ok () ->
      match Sdl.create_window_and_renderer ~w:win_w ~h:win_h Sdl.Window.opengl with
      | Error(`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
      | Ok (r,w) -> r,w
  in
  let bg_tex = Pic.img_of_file "./WESTUS.PIC" |> make_texture r in
  let map = Game_map.map_of_file "./WESTUS.PIC" in
  let map_tex = Game_map.pic_of_map map |> make_texture r in
  let event = Sdl.Event.create () in
  let w, h = win_w * 256 / 320, win_h * 192 / 200 in
  let map_rect = Sdl.Rect.create ~x:0 ~y:0 ~w ~h in
  let rec loop () =
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
    if stop then () else
      let open Result.Infix in
      let _ =
        Sdl.render_clear r >>= fun () ->
        Sdl.render_copy r bg_tex >>= fun () ->
        Sdl.render_copy r map_tex ~dst:map_rect
      in
      Sdl.render_present r;
      Sdl.delay 10l;
      loop ()
  in
  loop ();
  Sdl.destroy_renderer r;
  Sdl.destroy_window window;
  Sdl.quit ();
  exit 0


