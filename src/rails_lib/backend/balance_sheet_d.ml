open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = {
  operating_funds: int;
  treasury_stock: int;
  other_rr_stock: int;
  facilities: int;
  industries: int;
  real_estate: int;
  track_miles: int;
  track: int;
  rolling_stock: int;
  outstanding_loans: int;
  stockholders_equity: int;
} [@@deriving yojson]

let default = {
  operating_funds=1000;
  treasury_stock=0;
  other_rr_stock=0;
  facilities=0;
  industries=0;
  real_estate=0;
  track_miles=0;
  track=0;
  rolling_stock=0;
  outstanding_loans = -500;
  stockholders_equity = -500;
}

