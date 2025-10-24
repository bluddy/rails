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

