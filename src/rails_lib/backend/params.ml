open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* Parameters to be easily passed to child functions *)

type t = {
  year: int;
  year_start: int;
  num_fiscal_periods: int; (* Counts, like year, number of fiscal periods *)
  fiscal_period: [`First | `Second];
  climate: Climate.t;
  west_us_route_done: bool;
  region: Region.t;
  options: B_options.t;
  mutable cycle: int; (* ongoing counter used for all sorts of stuff *)
  mutable time: int;  (* In-game time, resets at end of fin period *)
} [@@deriving yojson]
