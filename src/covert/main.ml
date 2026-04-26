open Arg

module Pani_render = Engine.Pani_render
module Pani = Engine.Pani
module Mainloop = Engine.Mainloop

type actions = [ `Font | `Pic | `Cat | `Pani | `Game | `LoadGame]

let file = ref ""
let file_slot = ref 0
let mode : actions ref = ref `Game
let dump = ref false
let debugger = ref false
let zoom = ref 3
let adjust_ar = ref false
let shader = ref "test"

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
    "--cat", String (set `Cat), "Write files in .CAT file";
    "--pani", String (set `Pani), "Run the PANI file";
    "--dump", Set dump, "Dump the file";
    "--debug", Set debugger, "Run the debugger";
    "--load", Int (set_slot `LoadGame), "Load a save file";
    "--zoom", Int (fun x -> zoom := x), "Set zoom (default =3)";
    "--adjust-ar", Set adjust_ar, "Adjust aspect ratio";
    "--shader", String (fun s -> shader := s), "Shader name (default=test, looks in shaders/*.glsl)";
  ]

let main () =
  parse arglist (fun _ -> ()) "Usage";
  match !mode with
  | `Font -> Fonts.main !file
  | `Pic  -> Engine.Pic.png_of_file !file | `Cat -> Engine.Cat_file.of_file ~dump:true !file |> ignore
  | `Pani when !debugger && !dump ->
      Mainloop.main @@ Pani_render.debugger ~dump:true ~filename:!file
  | `Pani when !dump -> Pani.dump_file !file
  | `Pani when !debugger ->
      Mainloop.main @@ Pani_render.debugger ~filename:!file
  | `Pani ->
      Mainloop.main @@ Pani_render.standalone ~filename:!file
  | `Game -> Game_modules.run ~zoom:!zoom ~adjust_ar:!adjust_ar ~shader:!shader ()
  | `LoadGame -> Game_modules.run ~load:!file_slot ~zoom:!zoom ~adjust_ar:!adjust_ar ~shader:!shader ()

