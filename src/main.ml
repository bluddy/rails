open Arg

let font_file = ref ""
let pic_file = ref ""
let pani_file = ref ""
let dump = ref false

let arglist =
  [
    "--font", Set_string font_file, "Run the specified font";
    "--pic", Set_string pic_file, "Run the pic";
    "--pani", Set_string pani_file, "Run the pani";
    "--dump", Set dump, "Dump the file";
  ]

let () =
  parse arglist (fun _ -> ()) "Usage";
  if !font_file <> "" then
    Font.main !font_file
  else if !pic_file <> "" then
    Pic.png_of_file !pic_file
  else if !pani_file <> "" && !dump then
    Pani.main !pani_file
  else if !pani_file <> "" then
    Ui.main `Pani ~filename:!pani_file
  else
    Ui.main `MapDemo ~filename:""
