open Containers

type t =
  | Panic
  | Recession
  | Normal
  | Prosperity
  | Boom
  [@@deriving enum, yojson, show{with_path=false}]

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
  let roll () = Random.int 6 random + (to_enum v) - 2 in
  (* -2 to 4 + climate = -2 to 8 *)
  (* 6/11 to go up slightly, 3/11 to go down slightly, 2/11 to go down a lot *)
  match v with
  | _ when change_roll > 0 -> v
  | _ when roll () < 4 -> incr v
  | Boom -> Panic (* Crash *)
  | Prosperity -> Recession (* Minor crash *)
  | Normal -> Recession
  | Recession -> Panic
  | Panic -> Panic

