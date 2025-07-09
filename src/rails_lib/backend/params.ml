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

let make ?(year_start=1830) ?reality_levels ?difficulty ?(region=Region.WestUS) () =
{
  year=year_start;
  year_start;
  num_fiscal_periods=0;
  current_period=`First;
  climate=Normal;
  west_us_route_done=false;
  region;
  options=B_options.make ?reality_levels ?difficulty ();
  time=0;
  cycle=0;
}

let current_period v = v.current_period

let last_period v = match v.current_period with `First -> `Second | `Second -> `First

let next_period = last_period

let age v = v.year - v.year_start

