open Containers
open Tsdl
module Ndarray = Owl_base_dense_ndarray.Generic

module R = Renderer

let create_pixels (w,h) =
  Ndarray.create Int8_unsigned [|h;w;4|] 0

let main () =
  let win = R.create 320 200 ~zoom:2. in

  let bg_tex = Pic.img_of_file "./WESTUS.PIC" |> R.Texture.make win in
  (* Map area: 256 * 192 *)
  let map = Game_map.map_of_file "./WESTUS.PIC" in
  let map_tex = Game_map.pic_of_map map |> R.Texture.make win in
  let event = Sdl.Event.create () in
  let fonts = Font.load_all () in
  let font = fonts.(0) in

  let pixels = create_pixels (320-256,192) in
  Font.write ~font "Testing" ~pixels;
  let text_tex = pixels |> R.Texture.make win in

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
    let open Result.Infix in
    if stop then Result.return () else
      let* () = Sdl.render_clear win.renderer in
      let* () = Renderer.render win bg_tex in
      let* () = Renderer.render win map_tex in
      let* () = Renderer.render win text_tex in

      Sdl.render_present win.renderer;
      Sdl.delay 10l;
      loop ()
  in
  ignore(loop ());
  Sdl.destroy_renderer win.renderer;
  Sdl.destroy_window win.window;
  Sdl.quit ();
  exit 0


