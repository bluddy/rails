open Containers
open Tsdl
module Ndarray = Owl_base_dense_ndarray.Generic

let sdl_texture_of_ndarray renderer arr =
  let w, h = Ndarray.nth_dim arr 0, Ndarray.nth_dim arr 1 in
  let open Result.Infix in
  Sdl.create_rgb_surface_with_format_from (Bigarray.reshape_1 arr (w*h*3))
    ~w ~h ~depth:3 ~pitch:(w*3) Sdl.Pixel.format_rgb888
  >>= fun surf ->
  Sdl.create_texture_from_surface renderer surf

let main () =
  let win, renderer =
    match Sdl.init Sdl.Init.video with
    | Error(`Msg e) -> Sdl.log "Init error: %s" e; exit 1
    | Ok () ->
      match Sdl.create_window_and_renderer ~w:640 ~h:480 Sdl.Window.opengl with
      | Error(`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
      | Ok (r,w) -> r,w
  in
  let west_us_pic = Pic.load_to_bigarray "./WESTUS.PIC" in
  let texture =
    match sdl_texture_of_ndarray renderer west_us_pic with
    | Error(`Msg e) -> Sdl.log "Couldn't create texture from surface: %s" e; exit 1
    | Ok t -> t
  in
  let rec loop () =
    let open Result.Infix in
    let _ =
      Sdl.render_clear renderer >>= fun () ->
      Sdl.render_copy renderer texture
    in
    Sdl.delay 3000l;
    loop ()
  in
  loop
  (* Sdl.destroy_window w;
  Sdl.quit ();
  exit 0 *)


