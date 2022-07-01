open Containers
open Sexplib.Std

type t = {
  money: int; (* x1000 *)
  track_length: int;
}
[@@deriving sexp]

let default = {
  money = 1000;
  track_length = 0;
}

let get_money v = v.money

let track_length v = v.track_length

let add_track ~length v = {v with track_length = v.track_length + length}

