open Arg

type actions = [ `Font | `Pic | `Pani | `City | `Game | `LoadGame]

let file = ref ""
let file_slot = ref 0
let mode : actions ref = ref `Game
let dump = ref false
let debugger = ref false
let zoom = ref 3
let adjust_ar = ref false

let set v f =
  file := f;
  mode := v

let set_slot v n =
  file_slot := n;
  mode := v

let arglist =
  [
    "--font", String (set `Font), "Run the specified font";
    "--pic", String (set `Pic), "Convert .PIC to png";
    "--pani", String (set `Pani), "Run the PANI file";
    "--city", String (set `City), "Dump city info file";
    "--dump", Set dump, "Dump the file";
    "--debug", Set debugger, "Run the debugger";
    "--load", Int (set_slot `LoadGame), "Load a save file";
    "--zoom", Int (fun x -> zoom := x), "Set zoom (default =3)";
    "--adjust-ar", Set adjust_ar, "Adjust aspect ratio";
  ]

let main () =
  parse arglist (fun _ -> ()) "Usage";
  match !mode with
  | `Font -> Fonts.main !file
  | `Pic  -> Pic.png_of_file !file
  | `Pani when !debugger && !dump ->
      let sound_engine = Sound.init () in
      Mainloop.main @@ Pani_render.debugger ~sound_engine ~dump:true ~filename:!file
  | `Pani when !dump -> Pani.dump_file !file
  | `Pani when !debugger ->
      let sound_engine = Sound.init () in
      Mainloop.main @@ Pani_render.debugger ~sound_engine ~filename:!file
  | `Pani ->
      let sound_engine = Sound.init () in
      Mainloop.main @@ Pani_render.standalone ~sound_engine ~filename:!file
  | `City -> Mapgen.load_city_list WestUS |> ignore
  | `Game -> Game_modules.run ~zoom:!zoom ~adjust_ar:!adjust_ar ()
  | `LoadGame -> Game_modules.run ~load:!file_slot ~zoom:!zoom ~adjust_ar:!adjust_ar ()

