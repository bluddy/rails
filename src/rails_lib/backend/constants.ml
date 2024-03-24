
(* Time *)
(* Each time period is both 2 years and a 24 hour day *)

let player = 0
let num_players = 4

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
