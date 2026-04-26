
(* Time *)
(* Each time period is both 2 years and a 24 hour day *)

let scr_width = 320
let scr_height = 200

let tile_dim = 16
let tile_hdim = tile_dim / 2
let tile_w = tile_dim
let tile_h = tile_dim
let train_max_size = 8

module Cycles = struct
  (* Cycle counts to perform some tasks *)
end

module Pani = struct
  let wood_bridge = "data/WOOD2.PAN"
  let iron_bridge = "data/IRONM.PAN"
  let flood_us = "FLOODM.PAN"
  let flood_eu = "ENGFLDM.PAN"
  let wreck = "WRECKM.PAN"
  let title = "TITLEM.PAN"
end

module Intro = struct
  let wait_time = 2 (* secs *)
end

module HallOfFame = struct
  let max_entries = 5
end

module Save = struct
  let version = 1
end

module Sound = struct
end

