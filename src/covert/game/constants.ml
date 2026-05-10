
(* Time *)
(* Each time period is both 2 years and a 24 hour day *)

let scr_width = 320
let scr_height = 200

let cursor_flash_time = 500 (* ms *)

let num_crimes = 16
let num_regions = 3
let num_orgs = 16 (* per game, not total *)
let num_mms = 26
let num_locs = 16
let max_crime_steps = 6

module Cycles = struct
  (* Cycle counts to perform some tasks *)
end

module Pani = struct
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

