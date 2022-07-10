open Sexplib.Std

(**
   Backend options
**)

type speed =
  [`Frozen | `Slow | `Moderate | `Fast | `Turbo]
  [@@deriving enum, eq, show, sexp]

  (* multiplier for speed, relationship between rates *)
let delay_mult_of_speed = function
  | `Frozen -> 1000000 (* frozen *)
  | `Slow -> 30
  | `Moderate -> 9
  | `Fast -> 3
  | `Turbo -> 1 (* approximate. "As fast as possible" in original *)

type reality_level =
  [`Dispatcher_ops | `Complex_economy | `Cutthroat_competition]
  [@@deriving enum, eq, show, sexp]

module RealityLevels = Bitset.Make(struct
  type t = reality_level
  let to_enum = reality_level_to_enum
  let of_enum = reality_level_of_enum
  let last = `Cutthroat_competition
end)

type difficulty =
  [ `Diff25 | `Diff50 | `Diff75 | `Diff100 ]
  [@@deriving sexp]

type t = {
  speed: speed;
  reality_levels: RealityLevels.t;
  difficulty: difficulty;
} [@@deriving sexp]

let default =
  {
    speed=`Moderate;
    reality_levels=RealityLevels.of_list
      [`Dispatcher_ops; `Complex_economy; `Cutthroat_competition];
    difficulty=`Diff100;
  }


