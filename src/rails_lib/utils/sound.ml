open! Containers

include Engine.Sound

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

let play_sound ?loop ?time sound v =
  play_sound ?loop ?time (Sound.to_string sound) v

let queue_sound ?loop ?time sound v =
  queue_sound ?loop ?time (Sound.to_string sound) v

let play_music music v =
  play_music (Music.to_string music) v

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

let pani_create ?dump ?debug ?input ?sound sound_engine filename =
  pani_create ?dump ?debug ?input ?sound:(Option.map Music.to_string sound) sound_engine filename

