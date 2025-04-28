open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Utils.Infix

module C = Constants
module Vector = Utils.Vector

let src = Logs.Src.create "backend_low" ~doc:"Backend_low"
module Log = (val Logs.src_log src: Logs.LOG)

type route = (Utils.loc * Utils.loc)
             [@@deriving yojson]

type player = {
  cash: int; (* all x1000 *)
  bonds: int;
  opponent: Opponent.t;
  build_order: (Utils.loc * Utils.loc) option;  (* order given to subservient company *)
  yearly_income: int; (* rough estimation of 8 * yearly income *)
  net_worth: int;
} [@@deriving yojson]

  (* Global AI data *)
type t = {
  routes: route Vector.vector;
  cities_to_ai: int Loc_map.t;
} [@@deriving yojson]

let default_t = {
  routes = Vector.create ();
  cities_to_ai = Loc_map.empty;
}

    (* Update valuation only for AI players *)
let update_valuation player stocks v =
  let loans = v.bonds / 10 in
  let cash = v.cash / 10 in
  let stock_value = Stock_market.total_owned_stock_value player stocks in
  let net_worth = cash - loans + v.yearly_income * 2 + stock_value in
  {v with net_worth}

