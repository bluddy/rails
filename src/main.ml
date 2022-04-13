open Arg
open Containers
open Tsdl

type actions = [ `Font | `Pic | `Pani | `City | `MapDemo | `Game]

let file = ref ""
let mode : actions ref = ref `Game
let dump = ref false

let set v f =
  file := f;
  mode := v

let arglist =
  [
    "--font", String (set `Font), "Run the specified font";
    "--pic", String (set `Pic), "Convert .PIC to png";
    "--pani", String (set `Pani), "Run the PANI file";
    "--city", String (set `City), "Dump city info file";
    "--demo", String (set `MapDemo), "Run map demo";
    "--dump", Set dump, "Dump the file";
  ]

module R = Renderer

let init_mapdemo ~filename win =
  (* Draw the mapdemo *)
  let _file = filename in
  let bg_tex = Pic.img_of_file "./WESTUS.PIC" |> R.Texture.make win in
  (* Map area: 256 * 192 *)
  let map = Gmap.of_file ~area:Gmap.WestUS ~seed:0 "./WESTUS.PIC" in
  let map_tex = Gmap.to_img map |> R.Texture.make win in
  (* let fonts = Fonts.load () in *)

  (* Draw fonts *)
  (* let pixels = create_pixels (320-256,192) in *)
  let pixels = Pic.create_rgb_img (320,192) in
  (* let _ = *)
  (*   Array.foldi (fun (x, y) i font -> *)
  (*     Font.write ~font (Printf.sprintf "Font%d\n" i) ~pixels ~x ~y) *)
  (*   (0, 0) *)
  (*   fonts *)
  (* in *)
  let text_tex = R.Texture.make win pixels in

  let update () _ = (), false in

  let render () =
    let _ =
      let open Result.Infix in
      let* () = Sdl.render_clear win.R.renderer in
      let* () = Renderer.render win bg_tex in
      let* () = Renderer.render win map_tex in
      let* () = Renderer.render win ~x:257 text_tex in
      Result.return ()
    in ()
  in
  (), Graphics.{update; render}

let init_pani win ~filename =
  let stream = Pani.stream_of_file filename in
  let pani_v = Pani.of_stream stream in
  let pics_tex = pani_v.pics |>
    Array.map (function
      | None -> None
      | Some ndarray -> Some (R.Texture.make win ndarray))
  in

  let last_state = ref `Timeout in
  let last_time = ref @@ Sdl.get_ticks () in
  let update_delta = 10l in

  let update () _ =
    begin match !last_state with
    | `Done | `Error -> ()
    | _ ->
        let time = Sdl.get_ticks () in
        let open Int32 in
        if time - !last_time > update_delta
        then (
          last_time := time;
          last_state := Pani_interp.step pani_v;
        )
    end;
    (), false
  in

  let render () =
    (* let open Result.Infix in *)
    let () = ignore(Sdl.render_clear win.R.renderer) in

    (* Draw backgrounds *)
    let () =
      List.fold_right (fun Pani_interp.{x;y;pic_idx} _ ->
        match pics_tex.(pic_idx) with
        | None -> failwith @@ Printf.sprintf "No texture %d" pic_idx
        | Some tex ->
            ignore(Renderer.render win ~x ~y tex)
      )
      pani_v.backgrounds
      ()
    in
    (* Draw all pictures *)
    Iter.fold (fun _acc i ->
      match Pani_interp.anim_get_pic pani_v i with
      | None -> ()
      | Some pic_idx ->
          match pics_tex.(pic_idx) with
          | None -> failwith @@ Printf.sprintf "No pic_idx %d" pic_idx
          | Some pic_tex ->
            let x, y = Pani_interp.calc_anim_xy pani_v i in
            ignore(Renderer.render win ~x ~y pic_tex)
    )
    ()
    Iter.(0 -- 50)

  in 
  ((), Graphics.{update; render})

let () =
  parse arglist (fun _ -> ()) "Usage";
  match !mode with
  | `Font -> Fonts.main !file
  | `Pic  -> Pic.png_of_file !file
  | `Pani when !dump -> Pani.main !file
  | `Pani -> Graphics.main @@ init_pani ~filename:!file
  | `City -> Mapgen.load_city_list WestUS ~debug:true |> ignore
  | `MapDemo -> Graphics.main @@ init_mapdemo ~filename:!file
  | `Game -> Game.run ()

