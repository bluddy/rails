open Tsdl
open Tsdl_mixer

type sound =
  | Anim_bridge_metal
  | Anim_bridge_wood
  | Anim_train_sink
  | Broker
  | End_period_short_1
  | End_period_short_2
  | End_period_short_3
  | End_period_short_4
  | Main_theme
  | Remove_track
  | Station_build
  | Track_build
  | Train_delivery_bell
  | Train_horn
  | Train_horn_double
  | Train_horn_short
  | Train_horn_long
  | Train_horn_very_long
  | Train_woosh
  [@@deriving show {with_path=false}, eq, ord]

let show_sound sound = show_sound sound |> String.lowercase_ascii

let sound_of_str = function
  | "anim_bridge_metal" -> Anim_bridge_metal
  | "anim_bridge_wood" -> Anim_bridge_wood
  | "anim_train_sink" -> Anim_train_sink
  | "broker" -> Broker
  | "end_period_short_1" -> End_period_short_1
  | "end_period_short_2" -> End_period_short_2
  | "end_period_short_3" -> End_period_short_3
  | "end_period_short_4" -> End_period_short_4
  | "main_theme" -> Main_theme
  | "remove_track" -> Remove_track
  | "station_build" -> Station_build
  | "track_build" -> Track_build
  | "train_delivery_bell" -> Train_delivery_bell
  | "train_horn" -> Train_horn
  | "train_horn_double" -> Train_horn_double
  | "train_horn_short" -> Train_horn_short
  | "train_horn_long" -> Train_horn_long
  | "train_horn_very_long" -> Train_horn_very_long
  | "train_woosh" -> Train_woosh
  | s -> failwith @@ Printf.sprintf "Not a sound file %s" s

module SoundMap = Map.Make(struct type t = sound let compare = compare end)

type t = {
  sounds: Mixer.chunk SoundMap.t;
}

let init () =
  Sdl.init Sdl.Init.audio |> ignore;
  Mixer.open_audio 44100 Mixer.default_format 2 1024 |> ignore;
  let files = Utils.files_of_dir "sound" in
  let sound_files = List.filter (fun file ->
    String.equal (Filename.extension file) ".ogg") files
  in
  let sounds =
    List.fold_left (fun acc file ->
      let sound = Mixer.load_wav file |> Result.get_ok in
      let sound_name = Filename.basename file
        |> Filename.chop_extension
        |> sound_of_str
      in
      SoundMap.add sound_name sound acc)
    SoundMap.empty
    sound_files
  in
  {
    sounds;
  }

let play sound_name v =
  let sound = SoundMap.find sound_name v.sounds in
  Mixer.play_channel (-1) sound

