open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = {
  treasury_shares: int;
  other_rr_shares: int;
  public_shares: int;
  total_shares: int;
  share_price: int;
} [@@ deriving yojson]

let default_for_player difficulty =
  let share_price =
    B_options.difficulty_to_enum difficulty + 7
  in
  {
    treasury_shares = 0;
    other_rr_shares = 0;
    public_shares = 100;
    total_shares = 100;
    share_price;
  }

let compute_treasury_stock v =
  (* We need to account for the cost of selling all our stock 10k shares at a time *)
  let non_treasury_shares = v.total_shares - v.treasury_shares in
  let sale_rounds = v.treasury_shares / 10 in
  let rec loop i ~total ~share_price =
    if i < sale_rounds then
      let loss = (10 * share_price ) / (10 * i + non_treasury_shares + 10) in
      let share_price = share_price - loss in
      let total = total + share_price in
      loop (i + 1) ~total ~share_price
    else
      total
  in
  loop 0 ~total:0 ~share_price:v.share_price
