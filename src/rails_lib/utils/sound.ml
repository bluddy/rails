open! Containers
open Tsdl
open Tsdl_mixer

module Sound = struct

  type t =
    | Track_remove
    | Station_build
    | Track_build
    | Delivery_bell_very_low
    | Delivery_bell_low
    | Delivery_bell_mid
    | Delivery_bell_high
    | Train_horn
    | Train_horn_double
    | Train_horn_short
    | Train_horn_long
    | Train_horn_very_long
    | Train_woosh
    [@@deriving of_string, to_string, ord]

  let to_string sound = to_string sound |> String.lowercase_ascii

  let of_string str = Utils.upper_first str |> of_string

  let random_delivery_bell r =
    Random.choose_return [Delivery_bell_high; Delivery_bell_low; Delivery_bell_mid; Delivery_bell_very_low] r

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
    | End_period_song_main
    | Main_theme
    [@@deriving of_string, to_string, ord]

  let to_string sound = to_string sound |> String.lowercase_ascii

  let of_string str = Utils.upper_first str |> of_string


  module Map = Map.Make(struct type nonrec t = t let compare = compare end)
end

type sound_queue = {
  sound: Sound.t;
  time: int option;
  loop: int;
}

type t = {
  sounds: Mixer.chunk Sound.Map.t;
  mutable to_play: sound_queue list;
  music: Mixer.music Music.Map.t;
}

let play_sound ?(loop=0) ?time sound v =
  let sound = Sound.Map.find sound v.sounds in
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
  let music = Music.Map.find music v.music in
  Mixer.play_music music 1 |> ignore

let num_end_year_music = 4

let play_end_year_music num_fiscal_periods v =
  let open Music in
  let i = num_fiscal_periods mod num_end_year_music in
  let music =
    [ End_period_short_1; End_period_short_2; End_period_short_3; End_period_short_4]
  in
  if i >= 0 && i < num_end_year_music then (
    let music = List.nth music i in
    play_music music v
  )

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

