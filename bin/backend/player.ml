open Containers

type t = {
  money: int; (* x1000 *)
  bonds: int;
  yearly_interest_payment: int;
  owned_shares: int;
  shares: int;
  share_price: int;
  track_length: int;
  net_worth: int;
}
[@@deriving yojson]

let default difficulty = {
  money = 1000;
  bonds = 500;
  yearly_interest_payment=20;
  owned_shares=0;
  shares=100;
  share_price=B_options.difficulty_to_enum difficulty + 7;
  track_length = 0;
  net_worth=50;
}

let get_money v = v.money

let track_length v = v.track_length

let add_track ~length v = {v with track_length = v.track_length + length}

