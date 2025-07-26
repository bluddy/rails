open Arg

type actions = [ `Font | `Pic | `Pani | `City | `Game | `LoadGame]

let file = ref ""
let mode : actions ref = ref `Game
let dump = ref false
let debugger = ref false

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
    "--debug", Set debugger, "Run the debugger";
    "--load", String (set `LoadGame), "Load a save file";
  ]

let main () =
  parse arglist (fun _ -> ()) "Usage";
  match !mode with
  | `Font -> Fonts.main !file
  | `Pic  -> Pic.png_of_file !file
  | `Pani when !dump -> Pani.dump_file !file
  | `Pani when !debugger -> Mainloop.main @@ Pani_render.debugger ~filename:!file
  | `Pani -> Mainloop.main @@ Pani_render.standalone ~filename:!file
  | `City -> Mapgen.load_city_list WestUS |> ignore
  | `Game -> Game_modules.run ()
  | `LoadGame -> Game_modules.run ~load:!file ()

