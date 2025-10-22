open Tsdl
open Tsdl_mixer

type t = {
  sounds: (string, Mixer.chunk) Hashtbl.t;
}

let init () =
  Sdl.init Sdl.Init.audio |> ignore;
  Mixer.open_audio 44100 Mixer.default_format 2 1024 |> ignore;
  let files = Utils.files_of_dir "sound" in
  let sound_files = List.filter (fun file ->
    String.equal (Filename.extension file) ".ogg") files
  in
  let sound_h = Hashtbl.create 10 in
  List.iter (fun file ->
    let sound = Mixer.load_wav file |> Result.get_ok in
    let basename = Filename.basename file |> Filename.chop_extension in
    Hashtbl.replace sound_h basename sound)
  files;
  {
    sounds=sound_h;
  }




