
(* Time *)
(* Each time period is both 2 years and a 24 hour day *)

let player = 0
let max_num_players = 4

let tick_ms = 15 (* ms *)

let year_ticks = 2032 (* really 170*12 = 2040 is new year, but any time we go over 2032 we stop the year *)
let month_ticks = 170

let tile_dim = 16
let tile_hdim = tile_dim / 2
let tile_w = tile_dim
let tile_h = tile_dim
let train_max_size = 8

let car_amount = 160
let car_cost = 5 (* cost of a car *)
let car_full_demand = 64 

(* How much to divide by to get a human-readable amount of goods *)
let goods_div = 4

let speed_mult = 5 (* multiply speed by this *)
let min_maintenance_cost = 2

let tunnel_min_height = 80 (* height needed for tunnel *)
let tunnel_max_length = 9
let tunnel_cost = 20 (* per mile *)

let draw_margin = 4

let draw_buffer_len = 5

let fast_message_time = 40
let slow_message_time = 150

let menu_h = 8

let bond_value = 500
let max_cash_for_bankruptcy = 500
let min_bonds_for_bankruptcy = 500
let max_interest_rate = 9 (* At 9%, we can no longer sell bonds *)

let num_buy_shares = 10

module Cycles = struct
  (* Cycle counts to perform some tasks *)
  let periodic_maintenance = 1024
  let priority_delivery = 8
  let background_update = 16

  let ai_update = background_update
  (* In the original game, we do slices of 1/32 stations. No need *)
  let station_supply_demand = background_update * 32 (* 512 *)
  (* Since we don't spread out the supply addition, decay happens in the same cycle *)
  let supply_decay = 512
end

