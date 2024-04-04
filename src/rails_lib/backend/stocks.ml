open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module C = Constants

module IntMap = Utils.IntMap

type t = {
  player_idx: int;
  owned_shares: int IntMap.t; (* how many shares are owned in each company *)
  total_shares: int;
  share_price: int;
} [@@ deriving yojson]

let starting_share_price difficulty = 
    B_options.difficulty_to_enum difficulty + 7

let default_for_player ~player difficulty =
  {
    player_idx = player;
    owned_shares = IntMap.singleton 0 0;
    total_shares = 100;
    share_price = starting_share_price difficulty;
  }

let owned_shares v player_idx =
  IntMap.get_or ~default:0 player_idx v.owned_shares

let treasury_shares v = owned_shares v v.player_idx

let non_treasury_shares v = v.total_shares - treasury_shares v

let total_shares v = v.total_shares

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

let compute_treasury_stock v =
  (* How much the player owns in itself value-wise *)
  compute_owned_share_value ~total_shares:v.total_shares ~owned_shares:(treasury_shares v) ~share_price:v.share_price

let _add_shares owned_shares ~target_idx ~num_shares =
  IntMap.update target_idx (function | Some x -> Some(x + num_shares) | _ -> None) owned_shares

let add_shares v ~target_idx ~num_shares =
  let owned_shares = _add_shares v.owned_shares ~target_idx ~num_shares in
  {v with owned_shares}

let remove_shares v ~target_idx ~num_shares =
  add_shares v ~target_idx ~num_shares:(-num_shares)

let _set_shares owned_shares ~target_idx ~num_shares =
  IntMap.add target_idx num_shares owned_shares

let set_shares v ~target_idx ~num_shares =
  let owned_shares = _set_shares v.owned_shares ~target_idx ~num_shares in
  {v with owned_shares}

let num_owned_companies v =
  IntMap.fold (fun idx shares acc -> if v.player_idx <> idx && shares > 0 then acc + 1 else acc) v.owned_shares 0

