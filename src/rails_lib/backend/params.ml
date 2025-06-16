open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* Parameters to be easily passed to child functions *)

type t = {
  year: int;
  year_start: int;
  num_fiscal_periods: int; (* Counts, like year, number of fiscal periods *)
  current_period: [`First | `Second];
  climate: Climate.t;
  west_us_route_done: bool;
  region: Region.t;
  options: B_options.t;
  mutable cycle: int; (* ongoing counter used for all sorts of stuff *)
  mutable time: int;  (* In-game time, resets at end of fin period *)
} [@@deriving yojson]

let default = {
  year=1830;
  year_start=1830;
  num_fiscal_periods=0;
  current_period=`First;
  climate=Normal;
  west_us_route_done=false;
  region=Region.WestUS;
  options=B_options.default;
  time=0;
  cycle=0;
}

let current_period v = v.current_period
let last_period v = match v.current_period with `First -> `Second | `Second -> `First
let next_period = last_period
