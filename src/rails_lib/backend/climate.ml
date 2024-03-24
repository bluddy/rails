
type t =
  | Panic
  | Recession
  | Normal
  | Prosperity
  | Boom
  [@@deriving enum, yojson, show{with_path=false}]

let default = Normal

(* TODO: west us should have lower rates *)
let interest_rate climate outstanding =
  let base_rate = match climate with
  | Boom -> 2
  | Prosperity -> 3
  | Normal -> 4
  | Recession -> 5
  | Panic -> 6
  in
  let rate = base_rate + outstanding in
  if rate > 8 then None else Some rate
