
(**
   Backend options
**)

type speed =
  [`Frozen | `Slow | `Moderate | `Fast | `Turbo]
  [@@deriving enum, eq, show, yojson]

  (* multiplier for speed, relationship between rates *)
let delay_mult_of_speed = function
  | `Frozen -> 10000000 (* frozen *)
  | `Slow -> 30
  | `Moderate -> 9
  | `Fast -> 3
  | `Turbo -> 1 (* approximate. "As fast as possible" in original *)

type reality_level =
  [`DispatcherOps | `ComplexEconomy | `CutthroatCompetition]
  [@@deriving enum, eq, show, yojson]

module RealityLevels = Bitset.Make(struct
  type t = reality_level [@@deriving yojson]
  let to_enum = reality_level_to_enum
  let of_enum = reality_level_of_enum
  let last = `CutthroatCompetition
end)

type difficulty =
  [ `Investor | `Financier | `Mogul | `Tycoon ]
  [@@deriving yojson, enum]

let show_difficulty = function
  | `Investor -> "Investor"
  | `Financier -> "Financier"
  | `Mogul -> "Mogul"
  | `Tycoon -> "Tycoon"

type t = {
  speed: speed;
  reality_levels: RealityLevels.t;
  difficulty: difficulty;
} [@@deriving yojson]

let default =
  {
    speed=`Moderate;
    reality_levels=RealityLevels.of_list
      [`DispatcherOps; `ComplexEconomy; `CutthroatCompetition];
    difficulty=`Tycoon;
  }

let easy = function
  | `Investor | `Financier -> true
  | `Mogul | `Tycoon -> false

