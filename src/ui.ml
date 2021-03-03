open Containers
open Tsdl

let main () =
  let w =
    match Sdl.init Sdl.Init.video with
    | Error(`Msg e) -> Sdl.log "Init error: %s" e; exit 1
    | Ok () ->
      match Sdl.create_window ~w:640 ~h:480 "RAILS" Sdl.Window.opengl with
      | Error(`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
      | Ok w -> w
  in
  let x = Sdl.create_rgb_surface_from
  in
  Sdl.delay 3000l;
  Sdl.destroy_window w;
  Sdl.quit ();
  exit 0


