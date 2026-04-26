open! Containers
open Tsdl
open Tsdl_mixer

module StringMap = Utils.StringMap

type sound_queue = {
  sound: string;
  time: int option;
  loop: int;
}

type t = {
  sounds: Mixer.chunk StringMap.t;
  mutable to_play: sound_queue list;
  music: Mixer.music StringMap.t;
}

let play_sound ?(loop=0) ?time sound v =
  let sound = StringMap.find sound v.sounds in
  let time = Option.get_or ~default:(-1) time in
  let channel = -1 in
  Mixer.play_channel_timed channel sound loop time |> ignore

let init () =
  print_endline "Init audio...";
  Sdl.init Sdl.Init.audio |> ignore;
  Mixer.open_audio 48000 Mixer.default_format 2 1024 |> ignore;
  let get_files_of_dir dirname =
    let files = Utils.files_of_dir dirname in
    List.filter (fun file ->
    String.equal (Filename.extension file) ".ogg") files
  in
  let sound_files = get_files_of_dir "sound" in
  let sounds =
    List.fold_left (fun acc file ->
      let sound = Mixer.load_wav file |> Result.get_exn in
      let sound_name = Filename.basename file
        |> Filename.chop_extension
      in
      StringMap.add sound_name sound acc)
    StringMap.empty
    sound_files
  in
  let music_files = get_files_of_dir "music" in
  let music =
    List.fold_left (fun acc file ->
      let music = Mixer.load_mus file |> Result.get_exn in
      let music_name = Filename.basename file
        |> Filename.chop_extension
      in
      StringMap.add music_name music acc)
    StringMap.empty
    music_files
  in
  let v = {
      sounds;
      to_play=[];
      music;
    }
  in
  v

  (* queue for when the current sound finishes *)
let queue_sound ?(loop=0) ?time sound v =
  v.to_play <- v.to_play @ [{sound; time; loop}]

let play_music music v =
  let music = StringMap.find music v.music in
  Mixer.play_music music 1 |> ignore

let num_end_year_music = 4

let stop_music () = Mixer.halt_music () |> ignore

(* Call this to enable handling of queueing properly.
   Not required for most situations *)
let handle_tick v =
  if Mixer.playing None then () else (
  match v.to_play with
  | {sound; time; loop}::xs ->
      play_sound ~loop ?time sound v;
      v.to_play <- xs
  | _ -> ()
  )

let pani_create ?dump ?debug ?input ?sound sound_engine filename =
  let sound = match sound with
    | None -> None
    | Some sound ->
      Pani_render.
      {
        play_music=(fun () -> play_music sound sound_engine);
        stop_music=stop_music;
      }
      |> Option.some
  in
  Pani_render.create ?dump ?debug ?input ?sound filename

