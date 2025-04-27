(* Module for stockmarket *)
open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module C = Constants

module IntMap = Utils.IntMap

type t = {
  (* Who owns how many shares in what company. By default, all shares are public
     First level: owner. Second level: ownee
     *)
  ownership: (int IntMap.t) IntMap.t;
  prices: int IntMap.t; (* share prices *)
  totals: int IntMap.t; (* total number of shares *)
}

let default = {
  ownership=IntMap.empty;
  prices=IntMap.empty;
  totals=IntMap.empty;
}

let player_starting_share_price difficulty = 
    B_options.difficulty_to_enum difficulty + 7

let add_human_player ~player difficulty v =
  let totals = IntMap.add player C.Stock.starting_num v.totals in
  let prices = IntMap.add player (B_options.difficulty_to_enum difficulty + 7) v.prices in
  {v with totals; prices}

let owned_shares ~owner ~ownee v =
  match IntMap.get ~owner v.ownership with
  | Some ownees -> 
    Intmap.get_or ~default:0 ownee ownees
  | None -> 0

let treasury_shares player v = owned_shares ~owner:player ~ownee:player v

let total_shares player v = IntMap.get_or ~default:0 player v.totals

let non_treasury_shares player v = total_shares player v - treasury_shares player v

let share_price player v = IntMap.get_or ~default:0 player v.prices

let compute_owned_share_value ~total_shares ~owned_shares ~share_price =
  (* We need to account for the cost of selling all our stock 10k shares at a time *)
  let non_treasury_shares = total_shares - owned_shares in
  let sale_rounds = owned_shares / 10 in
  let rec loop i ~total ~share_price =
    if i < sale_rounds then
      let loss = (10 * share_price ) / (10 * i + non_treasury_shares + 10) in
      let share_price = share_price - loss in
      let total = total + share_price in
      loop (i + 1) ~total ~share_price
    else
      total
  in
  loop 0 ~total:0 ~share_price

let treasury_share_value player v =
  (* How much the player owns in himself value-wise *)
  let total_shares = total_shares player v in
  let treasury_shares = treasury_shares player v in
  let share_price = share_price player v in
  compute_owned_share_value ~total_shares ~owned_shares:treasury_shares ~share_price

let _add_shares owned_shares ~target_idx ~num_shares =
  IntMap.update target_idx (function | Some x -> Some(x + num_shares) | _ -> None) owned_shares

let add_shares ~owner ~ownee ~num v =
  let ownership =
    IntMap.update owner (function
      | Some ownees -> IntMap.incr ownee num ownees |> Option.some
      | None -> IntMap.empty |> IntMap.incr ownee num |> Option.some)
    v.ownership
  in
  {v with ownership}

let remove_shares ~owner ~ownee ~num v = add_shares ~owner ~ownee ~num:(-num) v

let _set_shares owned_shares ~target_idx ~num_shares =
  IntMap.add target_idx num_shares owned_shares

let set_shares ~owner ~ownee ~num v =
  let ownership =
    IntMap.update owner (function
      | Some ownees -> IntMap.add ownee num ownees |> Option.some
      | None -> IntMap.empty |> IntMap.add ownee num |> Option.some)
    v.ownership
  in
  {v with ownership}

let reset_owned_shares player v =
  let ownership = IntMap.remove player v.ownership in
  {v with ownership}

let set_total_shares ~player total_shares v =
  let totals = IntMap.add player total_shares v.totals in
  {v with totals}

let num_companies_owner_has_shares_in owner v =
  match IntMap.get owner v.ownership with
  | None -> 0
  | Some ownees ->
    IntMap.sum (fun player shares ->
      if owner <> player && shares > 0 then 1 else 0)
      ownees

