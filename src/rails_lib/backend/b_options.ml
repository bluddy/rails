
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
  [@@deriving enum, eq, yojson, show]

  (* TODO *)
let show_reality_level ?(pos=true) v = match v, pos with
  | `DispatcherOps, true -> "Dispatcher Ops"
  | `DispatcherOps, false -> "'No-collision' Operation"
  | `ComplexEconomy, true -> "Complex Economy"
  | `ComplexEconomy, false -> "Basic Economy"
  | `CutthroatCompetition, true -> "Cut-throat Competition"
  | `CutthroatCompetition, false -> "Friendly Competition"

let reality_levels = [`DispatcherOps; `ComplexEconomy; `CutthroatCompetition]

module RealityLevels = Bitset.Make(struct
  type t = reality_level [@@deriving yojson]
  let to_enum = reality_level_to_enum
  let of_enum = reality_level_of_enum
  let last = `CutthroatCompetition
end)

let reality_levels_default = RealityLevels.empty
  |> RealityLevels.add `DispatcherOps

type difficulty =
  [ `Investor | `Financier | `Mogul | `Tycoon ]
  [@@deriving yojson, enum, ord]

let difficulties = [`Investor; `Financier; `Mogul; `Tycoon]

let show_difficulty = function
  | `Investor -> "Investor"
  | `Financier -> "Financier"
  | `Mogul -> "Mogul"
  | `Tycoon -> "Tycoon"

let easy = function
  | `Investor | `Financier -> true
  | `Mogul | `Tycoon -> false

type t = {
  speed: speed;
  reality_levels: RealityLevels.t;
  difficulty: difficulty;
} [@@deriving yojson]

let investor v = match v.difficulty with | `Investor -> true | _ -> false

let make ?(reality_levels=reality_levels_default) ?(difficulty=`Financier) () =
  {
    speed=`Moderate;
    reality_levels;
    difficulty;
  }

let difficulty v = v.difficulty

let difficulty_enum v = difficulty_to_enum @@ difficulty v

let speed v = v.speed

let cutthroat v = RealityLevels.mem v.reality_levels `CutthroatCompetition

let dispatcher_ops v = RealityLevels.mem v.reality_levels `DispatcherOps
let complex_economy v = RealityLevels.mem v.reality_levels `ComplexEconomy 
let simple_economy v = not @@ complex_economy v

let compute_difficulty_pct difficulty reality_levels =
  let x = difficulty_to_enum difficulty + 1 in
  let x = if RealityLevels.mem reality_levels `DispatcherOps then x + 1 else x in
  let x = if RealityLevels.mem reality_levels `ComplexEconomy then x + 1 else x in
  let x = if RealityLevels.mem reality_levels `CutthroatCompetition then x + 1 else x in
  x * 10
  
