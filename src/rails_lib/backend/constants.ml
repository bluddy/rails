
(* Time *)
(* Each time period is both 2 years and a 24 hour day *)

let screen_width = 320
let screen_height = 200

let moi = Money.of_int

let player = Owner.create_human ()
let max_num_players = 4
let max_ai_players = 3
let max_num_cities = 100

let tick_ms = 15 (* ms *)

let year_ticks = 2032 (* really 170*12 = 2040 is new year, but any time we go over 2032 we stop the year *)
let fin_period_ticks = 4080
let month_ticks = 170

let tile_dim = 16
let tile_hdim = tile_dim / 2
let tile_w = tile_dim
let tile_h = tile_dim
let train_max_size = 8

let min_dist_btw_stations = 5 (* gt *)

let car_amount = 160
let car_cost = moi 5 (* cost of a car *)
let car_full_demand = 64 

(* How much to divide by to get a human-readable amount of goods *)
let goods_div = 4

let speed_mult = 5 (* multiply speed by this *)
let min_maintenance_cost = moi 2

let track_length = 2

let tunnel_min_height = 80 (* height needed for tunnel *)
let tunnel_max_length = 9
let tunnel_cost = moi 20 (* per mile *)

let draw_margin = 4

let draw_buffer_len = 5

let fast_message_time = 40
let slow_message_time = 150

let menu_h = 8

(* In the west US it's 1000, but the AI doesn't always use it in the original game *)
let bond_value = moi 500
let max_cash_for_bankruptcy = moi 500
let min_bonds_for_bankruptcy = moi 500
let max_interest_rate = 9 (* At 9%, we can no longer sell bonds *)

let num_buy_shares = 10

module Cycles = struct
  (* Cycle counts to perform some tasks *)
  let periodic_maintenance = 1024
  let climate_change = 1024
  let event_change = 1024
  let rare_bgnd_events = 8
  let background_update = 16

  let ai_update = background_update
  (* In the original game, we do slices of 1/32 stations. No need *)
  let station_supply_demand = background_update * 32 (* 512 *)
  (* Since we don't spread out the supply addition, decay happens in the same cycle *)
  let supply_decay = 512

  let ai_track = 16
  let ai_financial = 16
  let ai_financial_choice = 4 (* 0-2 AIs or 3=nothing *)
end

let priority_min_dist = 6
let priority_max_dist = 64
let priority_min_bonus = moi 20

let reference_year = 1740

let build_industry_mult = 40

let chance_destroy_resource = 9 (* 1 in 9 *)

let reference_year_map_dev = 1770

let young_station_age = 20

(* This is the number we roll to see if we maintain *)
let maintain_max_roll = 1250
let washout_max_roll = 1024

let track_maintenance_single = moi 1
let track_maintenance_double = moi 2

module Stock = struct
  let starting_num = 100
  let ai_share_price = moi 10
end

let ref_year_ai_route_value = 1710
let ref_year_ai_build_value = 1770

let ai_max_cash = moi 30000

let map_width = 256
let map_height = 192

let bridge_washout_grace_age = 5 (* How long before washouts can happen *)
let bridge_extra_roll = 100     (* Doesn't matter so much so long as it works for both iron and stone *)
let bridge_washout_tries = 16  (* How many times we try to hit a bridge *)
let iron_bridge_fail_odds = 4  (* The extra odds of an iron bridge failing *)
let stone_bridge_fail_odds = 8  (* The extra odds of a stone bridge failing *)

let newspaper_cost_ref_year = 1775

module Pani = struct
  let update_delta = 10
  let max_num_animations = 50
  let wood_bridge = "WOOD2.PAN"
  let iron_bridge = "IRONM.PAN"
  let flood_us = "FLOODM.PAN"
  let flood_eu = "ENGFLDM.PAN"
  let wreck = "WRECKM.PAN"
  let title = "TITLEM.PAN"
end

module Transition = struct
  let tick_delta = 10
  let step_pixels = 2500
end

module Intro = struct
  let wait_time = 2 (* secs *)
end

module RateWar = struct
  let loss_radius = 3 (* How far we remove player stuff *)
end

