open Containers
module Random = Utils.Random

type t =
  | Panic
  | Recession
  | Normal
  | Prosperity
  | Boom
  [@@deriving enum, yojson, show{with_path=false}]

type reason =
  | WesternGoldStrike
  | NewTerritory
  | InventionPatented
  | ForeignTradeUp
  | BankFailure
  | LaborViolence
  [@@deriving yojson]

let default = Normal

let weak_or_normal = function
  | Panic
  | Recession
  | Normal -> true
  | _ -> false

let strong = function
  | Prosperity
  | Boom -> true
  | _ -> false

let plus_4 climate = to_enum climate + 4

let modify random v =
  let change_roll =
    (* More likely to stay in panic *)
    let roll_max = match v with Panic -> 2 | _ -> 3 in
    Random.int roll_max random
  in
  let incr = function
    | Panic -> Recession
    | Recession -> Normal
    | Normal -> Prosperity
    | Prosperity -> Boom
    | Boom -> Boom
  in
  let incr_roll () = Random.int 6 random + (to_enum v) - 2 in
  (* -2 to 4 + climate = -2 to 8 *)
  (* 6/11 to go up slightly, 3/11 to go down slightly, 2/11 to go down a lot *)
  let down_reason () = Random.pick_array [| BankFailure; LaborViolence |] random in
  let up_reason () = Random.pick_array
    [| WesternGoldStrike; NewTerritory; InventionPatented; ForeignTradeUp |] random
  in
  match v with
  | _ when change_roll > 0 -> `Same
  | Boom when incr_roll () < 4 -> `Same
  | _ when incr_roll () < 4 -> `Change (incr v, up_reason ())
  | Panic -> `Same
  | Boom -> `Change(Panic, down_reason ()) (* Crash *)
  | Prosperity -> `Change(Recession, down_reason ()) (* Minor crash *)
  | Normal -> `Change(Recession, down_reason())
  | Recession -> `Change(Panic, down_reason())

let text1 = function
  | WesternGoldStrike -> " Western Gold Strike!", " New Lode Uncovered!"
  | NewTerritory -> " New Territory Settled", " Land Rush Expected."
  | InventionPatented -> " Invention Patented!",  " Perpetual Motion!"
  | ForeignTradeUp -> " Foreign Trade Up", " New Markets Opened"
  | BankFailure -> " Bank Failure!!", " Wall Street Worried."
  | LaborViolence -> " Labor Violence!", " Militia Called Out!"

let text2 = function
  | Panic -> " PANIC Strikes!!"
  | Recession -> " Recession Feared."
  | Normal -> " Moderation Expected"
  | Prosperity -> " Prosperity in Sight."
  | Boom -> " BOOM Times Ahead!"
