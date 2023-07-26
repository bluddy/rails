open Arg
open Containers
open Tsdl

type actions = [ `Font | `Pic | `Pani | `City | `Game | `LoadGame]

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
    "--dump", Set dump, "Dump the file";
    "--load", String (set `LoadGame), "Load a save file";
  ]

module R = Renderer

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

  let handle_tick () _ =
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
    ()
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
            ignore(R.Texture.render win ~x ~y tex)
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
            ignore(R.Texture.render win ~x ~y pic_tex)
    )
    ()
    Iter.(0 -- 50)

  in 
  let handle_event () _ = (), false in
  ((), Mainloop.{handle_tick; render; handle_event})

let main () =
  parse arglist (fun _ -> ()) "Usage";
  match !mode with
  | `Font -> Fonts.main !file
  | `Pic  -> Pic.png_of_file !file
  | `Pani when !dump -> Pani.main !file
  | `Pani -> Mainloop.main @@ init_pani ~filename:!file
  | `City -> Mapgen.load_city_list WestUS |> ignore
  | `Game -> Frontend.run ()
  | `LoadGame -> Frontend.run ~load:!file ()

