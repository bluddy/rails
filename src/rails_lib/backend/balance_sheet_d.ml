open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module M = Money

type t = {
  operating_funds: Money.t;
  treasury_stock: Money.t;
  other_rr_stock: Money.t;
  facilities: Money.t;
  industries: Money.t;
  real_estate: Money.t;
  track_miles: int;
  track: Money.t;
  rolling_stock: Money.t;
  outstanding_loans: Money.t;
  stockholders_equity: Money.t;
  end_of_year: bool;
} [@@deriving yojson]

let default = {
  operating_funds=Money.of_int 1000;
  treasury_stock=M.zero;
  other_rr_stock=M.zero;
  facilities=M.zero;
  industries=M.zero;
  real_estate=M.zero;
  track_miles=0;
  track=M.zero;
  rolling_stock=M.zero;
  outstanding_loans = Money.of_int @@ -500;
  stockholders_equity = Money.of_int @@ -500;
  end_of_year=false;
}

