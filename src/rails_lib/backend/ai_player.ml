open Containers
open Utils.Infix

module C = Constants

let src = Logs.Src.create "backend_low" ~doc:"Backend_low"
module Log = (val Logs.src_log src: Logs.LOG)

type t = {
  cash: int; (* all x1000 *)
  bonds: int;
  opponent: Opponent.t;
  build_order: (Utils.loc * Utils.loc) option;  (* order given to subservient company *)
  yearly_income: int; (* rough estimation of 8 * yearly income *)
} [@@deriving yojson]

    (* Update valuation only for AI players *)
let update_valuation players player_idx =
  let player = players.(player_idx) in
  let loans = player.m.bonds / 10 in
  let cash = player.m.cash / 10 in
  let income = match player.ai with
    | Some ai_info -> ai_info.yearly_income
    | _ -> 0
  in
  let stock_value = total_owned_stock_value players ~player_idx in
  let net_worth = cash - loans + income * 2 + stock_value in
  players.(player_idx) <- {player with m={player.m with net_worth}};
  ()
