open Arg

module Pani_render = Engine.Pani_render
module Pani = Engine.Pani
module Mainloop = Engine.Mainloop
module Renderer = Engine.Renderer

type actions = [ `Font | `Pic | `Cat | `Pani | `Game | `LoadGame]

let file = ref ""
let file_slot = ref 0
let mode : actions ref = ref `Game
let dump = ref false
let debugger = ref false
let zoom = ref (None : int option)
let adjust_ar = ref false
let shader = ref Renderer.Default
let audio = ref true
let in1 = ref 0
let in2 = ref 0

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
    "--zoom", Int (fun x -> zoom := Some x), "Display zoom multiplier (default: largest that fits the screen)";
    "--adjust-ar", Set adjust_ar, "Adjust aspect ratio";
    "--shader", String (fun s -> shader := Renderer.Named s), "Shader name (default: EGA shader matched to screen size, looks in shaders/*.glsl)";
    "--no-shader", Unit (fun () -> shader := Renderer.No_shader), "Disable shaders (raw pixels)";
    "--no-audio", Clear audio, "Disable audio";
    "--input", Tuple [Set_int in1; Set_int in2], "Address and value";
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
      Mainloop.main @@ Pani_render.standalone ~filename:!file ~input:[!in1, !in2]
  | `Game -> Game_modules.run ?zoom:!zoom ~adjust_ar:!adjust_ar ~audio:!audio ~shader:!shader ()
  | `LoadGame -> Game_modules.run ~load:!file_slot ?zoom:!zoom ~adjust_ar:!adjust_ar ~audio:!audio ~shader:!shader ()

