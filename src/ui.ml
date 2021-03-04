open Containers
open Tsdl
module Ndarray = Owl_base_dense_ndarray.Generic

let sdl_surface_of_ndarray arr =
  let w, h = Ndarray.nth_dim arr 0, Ndarray.nth_dim arr 1 in
  Sdl.create_rgb_surface_with_format_from (Bigarray.reshape_1 arr (w*h*3))
    ~w ~h ~depth:3 ~pitch:(w*3) Sdl.Pixel.format_rgb888

let main () =
  let w =
    match Sdl.init Sdl.Init.video with
    | Error(`Msg e) -> Sdl.log "Init error: %s" e; exit 1
    | Ok () ->
      match Sdl.create_window ~w:640 ~h:480 "RAILS" Sdl.Window.opengl with
      | Error(`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
      | Ok w -> w
  in
  let west_us_pic = Pic.load_to_bigarray "./WESTUS.PIC" |> sdl_surface_of_ndarray in
  Sdl.delay 3000l
  (* Sdl.destroy_window w;
  Sdl.quit ();
  exit 0 *)


