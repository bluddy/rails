open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

type assets = {
  operating_funds: int;
  treasury_stock: int;
  other_rr_stock: int;
  faciliies: int;
  industries: int;
  real_estate: int;
  track: int;
  rolling_stock: int;
} [@@deriving yojson]

type liabilities = {
  outstanding_loans: int;
  stockholders_equity: int;
} [@@deriving yojson]

type t = {
  assets: assets;
  liabilities: liabilities;
} [@@deriving yojson]

let default =
  let assets = {
    operating_funds=0;
    treasury_stock=0;
    other_rr_stock=0;
    faciliies=0;
    industries=0;
    real_estate=0;
    track=0;
    rolling_stock=0;
  }
  in
  let liabilities = {
    outstanding_loans=0;
    stockholders_equity=0;
  }
  in
  {
    assets;
    liabilities;
  }
