open Containers
open Tsdl
module Ndarray = Owl_base_dense_ndarray.Generic

module R = Renderer

let create_pixels (w,h) =
  Ndarray.create Int8_unsigned [|h;w;4|] 0

let setup_mapdemo win =
    (* Draw the mapdemo *)
    let bg_tex = Pic.img_of_file "./WESTUS.PIC" |> R.Texture.make win in
    (* Map area: 256 * 192 *)
    let map = Game_map.map_of_file "./WESTUS.PIC" in
    let map_tex = Game_map.pic_of_map map |> R.Texture.make win in
    let fonts = Font.load_all () in

    (* Draw fonts *)
    (* let pixels = create_pixels (320-256,192) in *)
    let pixels = create_pixels (320,192) in
    let _ =
      Array.foldi (fun (x, y) i font ->
        Font.write ~font (Printf.sprintf "Font%d\n" i) ~pixels ~x ~y)
      (0, 0)
      fonts
    in
    let text_tex = R.Texture.make win pixels in

    let render_fn () =
      let open Result.Infix in
      let* () = Sdl.render_clear win.renderer in
      let* () = Renderer.render win bg_tex in
      let* () = Renderer.render win map_tex in
      let* () = Renderer.render win ~x:257 text_tex in
      Result.return ()
    in
    render_fn


let main choice =
  let win = R.create 320 200 ~zoom:2. in
  let event = Sdl.Event.create () in

  let render_fn =
    match choice with
    | `Pani -> setup_mapdemo win
    | `MapDemo -> setup_mapdemo win
  in

  let rec event_loop render_fn =
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
    if stop then Result.return () else

      (* render whatever we want *)
      let _ = render_fn () in

      Sdl.render_present win.renderer;
      Sdl.delay 10l;
      event_loop render_fn 
  in
  ignore(event_loop render_fn);
  Sdl.destroy_renderer win.renderer;
  Sdl.destroy_window win.window;
  Sdl.quit ();
  exit 0


