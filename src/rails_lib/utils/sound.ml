open! Containers
open Tsdl
open Tsdl_mixer

module Sound = struct

  type t =
    | Track_remove
    | Station_build
    | Track_build
    | Train_delivery_bell
    | Train_horn
    | Train_horn_double
    | Train_horn_short
    | Train_horn_long
    | Train_horn_very_long
    | Train_woosh
    [@@deriving of_string, to_string, ord]

  let to_string sound = to_string sound |> String.lowercase_ascii

  let of_string str = Utils.upper_first str |> of_string

  module Map = Map.Make(struct type nonrec t = t let compare = compare end)

end

module Music = struct
  type t =
    | Anim_bridge_metal
    | Anim_bridge_wood
    | Anim_train_sink
    | Broker
    | End_period_short_1
    | End_period_short_2
    | End_period_short_3
    | End_period_short_4
    | Main_theme
    [@@deriving of_string, to_string, ord]

  let to_string sound = to_string sound |> String.lowercase_ascii

  let of_string str = Utils.upper_first str |> of_string

  module Map = Map.Make(struct type nonrec t = t let compare = compare end)
end

type t = {
  sounds: Mixer.chunk Sound.Map.t;
  music: Mixer.music Music.Map.t;
}

let init () =
  print_endline "Init audio...";
  Sdl.init Sdl.Init.audio |> ignore;
  Mixer.open_audio 48000 Mixer.default_format 2 32768 |> ignore;
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
        |> Sound.of_string
        |> Option.get_exn_or "Unknown sound file"
      in
      Sound.Map.add sound_name sound acc)
    Sound.Map.empty
    sound_files
  in
  let music_files = get_files_of_dir "music" in
  let music =
    List.fold_left (fun acc file ->
      let music = Mixer.load_mus file |> Result.get_exn in
      let music_name = Filename.basename file
        |> Filename.chop_extension
        |> Music.of_string
        |> Option.get_exn_or "Unknown music file"
      in
      Music.Map.add music_name music acc)
    Music.Map.empty
    music_files
  in
  {
    sounds;
    music;
  }

let play_sound ?(loop=0) sound v =
  (* We can discard the channel - we don't expect a sound to be stopped *)
  let sound = Sound.Map.find sound v.sounds in
  Mixer.play_channel (-1) sound loop |> ignore

let play_music music v =
  let music = Music.Map.find music v.music in
  Mixer.play_music music (-1) |> ignore

let stop_music () = Mixer.halt_music () |> ignore

