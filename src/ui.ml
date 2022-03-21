open Containers
open Tsdl
module Ndarray = Owl_base_dense_ndarray.Generic

module R = Renderer

let create_pixels (w,h) =
  Ndarray.create Int8_unsigned [|h;w;4|] 0

let setup_mapdemo win ~filename =
    (* Draw the mapdemo *)
    let _file = filename in
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

    let update_fn () = () in

    let render_fn () =
      let open Result.Infix in
      let* () = Sdl.render_clear win.renderer in
      let* () = Renderer.render win bg_tex in
      let* () = Renderer.render win map_tex in
      let* () = Renderer.render win ~x:257 text_tex in
      Result.return ()
    in
    update_fn, render_fn

let setup_pani win ~filename =
  let stream = Pani.stream_of_file filename in
  let pani_v = Pani.of_stream stream in
  let pics_tex = pani_v.pics |>
    Array.map (function
      | None -> None
      | Some ndarray -> Some (R.Texture.make win ndarray))
  in

  let last_state = ref `Timeout in
  let last_time = ref @@ Sdl.get_ticks () in
  let update_delta = 100l in

  let update_fn () =
    match !last_state with
    | `Done | `Error -> ()
    | _ ->
        let time = Sdl.get_ticks () in
        let open Int32 in
        if time - !last_time > update_delta
        then (
          last_time := time;
          last_state := Pani_interp.step pani_v
        ) else ()
  in

  let render_fn () =
    let open Result.Infix in
    let* () = Sdl.render_clear win.renderer in
    (* Draw background *)
    let* () =
      match pics_tex.(0) with
      | Some pic_tex ->
          Renderer.render win ~x:0 ~y:0 pic_tex
      | None -> Result.return ()
    in

    Iter.fold (fun _acc i ->
      match Pani_interp.anim_get_pic pani_v i with
      | None -> Result.return ()
      | Some pic_idx ->
          match pics_tex.(pic_idx) with
          | None -> failwith @@ Printf.sprintf "No pic_idx %d" pic_idx
          | Some pic_tex ->
            let x, y = Pani_interp.calc_anim_xy pani_v i in
            let* () = Renderer.render win ~x ~y pic_tex in
            Result.return ()
    )
    (Result.return ())
    Iter.(0 -- 50)
  in 
  update_fn, render_fn

let main choice ~filename =
  let win = R.create 320 200 ~zoom:2. in
  let event = Sdl.Event.create () in

  let render_fn, update_fn =
    match choice with
    | `Pani -> setup_pani win ~filename
    | `MapDemo -> setup_mapdemo win ~filename
  in

  let rec event_loop (last_time:int32) =
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
    let render_wait_time = 30l in
    if stop then Result.return () else

      let _ = update_fn () in
      let _ = render_fn () in

      let open Int32.Infix in
      let time = Sdl.get_ticks () in
      if time - last_time < render_wait_time then
        Sdl.delay (render_wait_time - time + last_time);

      Sdl.render_present win.renderer;
      event_loop time
  in
  ignore(event_loop @@ Sdl.get_ticks ());

  Sdl.destroy_renderer win.renderer;
  Sdl.destroy_window win.window;
  Sdl.quit ();
  exit 0


