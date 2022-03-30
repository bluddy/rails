open Arg

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

let () =
  parse arglist (fun _ -> ()) "Usage";
  match !mode with
  | `Font -> Font.main !file
  | `Pic  -> Pic.png_of_file !file
  | `Pani when !dump -> Pani.main !file
  | `Pani -> Graphics.main `Pani ~filename:!file
  | `City -> Mapgen.load_city_list !file
  | `MapDemo -> Graphics.main `MapDemo ~filename:!file
  | `Game -> Game.run ()
