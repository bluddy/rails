open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* Parameters to be easily passed to child functions *)

type t = {
  year: int;
  year_start: int;
  fiscal_period: [`First | `Second];
  climate: Climate.t;
  west_us_route_done: bool;
  region: Region.t;
  options: B_options.t;
} [@@deriving yojson]
