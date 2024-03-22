open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module C = Constants

type t = {
  player_idx: int;
  owned_shares: int array; (* how many shares are owned in each company *)
  total_shares: int;
  share_price: int;
} [@@ deriving yojson]

let default_for_player ~player difficulty =
  let share_price =
    B_options.difficulty_to_enum difficulty + 7
  in
  {
    player_idx = player;
    owned_shares = Array.make C.num_players 0;
    total_shares = 100;
    share_price;
  }

let treasury_shares v = v.owned_shares.(v.player_idx)

let get_owned_shares v player_idx = v.owned_shares.(player_idx)

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
  (* How much the player owns in itself *)
  compute_owned_share_value ~total_shares:v.total_shares ~owned_shares:(treasury_shares v) ~share_price:v.share_price

