
type t =
  | Panic
  | Recession
  | Normal
  | Prosperity
  | Boom
  [@@deriving enum, yojson, show{with_path=false}]

let default = Normal

let interest_rate climate region outstanding_bonds =
  let bond_val = match region with
    | Region.WestUS -> 1000 (* Special treatment *)
    | _ -> 500
  in
  let num_bonds = outstanding_bonds / bond_val in
  let base_rate = match climate with
  | Boom -> 2
  | Prosperity -> 3
  | Normal -> 4
  | Recession -> 5
  | Panic -> 6
  in
  base_rate + num_bonds
