
(* Time *)
(* Each time period is both 2 years and a 24 hour day *)

let player = 0
let max_num_players = 4
let max_num_cities = 100

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
  let rare_bgnd_events = 8
  let background_update = 16

  let ai_update = background_update
  (* In the original game, we do slices of 1/32 stations. No need *)
  let station_supply_demand = background_update * 32 (* 512 *)
  (* Since we don't spread out the supply addition, decay happens in the same cycle *)
  let supply_decay = 512
end

let priority_min_dist = 6
let priority_max_dist = 64
let priority_min_bonus = 20

let reference_year = 1740

let build_industry_mult = 40

let chance_destroy_resource = 9 (* 1 in 9 *)

let reference_year_map_dev = 1770

let young_station_age = 20

(* This is the number we roll to see if we maintain *)
let maintain_max_roll = 1250

let track_maintenance_single = 1
let track_maintenance_double = 2

module Stock = struct
  let starting_num = 100
end

let ref_year_ai_route_value = 1710

let ai_max_cash = 30000

