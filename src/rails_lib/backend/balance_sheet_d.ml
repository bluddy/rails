open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = {
  operating_funds: Money.t;
  treasury_stock: Money.t;
  other_rr_stock: Money.t;
  facilities: Money.t;
  industries: Money.t;
  real_estate: Money.t;
  track_miles: int;
  track: int;
  rolling_stock: Money.t;
  outstanding_loans: Money.t;
  stockholders_equity: Money.t;
} [@@deriving yojson]

let default = {
  operating_funds=Money.of_int 1000;
  treasury_stock=Money.of_int 0;
  other_rr_stock=Money.of_int 0;
  facilities=Money.of_int 0;
  industries=Money.of_int 0;
  real_estate=Money.of_int 0;
  track_miles=0;
  track=0;
  rolling_stock=Money.of_int 0;
  outstanding_loans = Money.of_int @@ -500;
  stockholders_equity = Money.of_int @@ -500;
}

