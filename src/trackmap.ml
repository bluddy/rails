open Containers

type t = {
  map: Track.t array;
  count: int;
  w: int;
  h: int;
}

let empty w h =
  let map = Array.make (w * h) Track.empty in
  let count = 0 in
  {map; w; h; count}

let calc_offset v x y = y * v.w + x

let get v x y = v.map.(calc_offset v x y)

let set v x y tile =
  v.map.(calc_offset v x y) <- tile

let pre_build_track v ~x ~y ~dir ~player =
  let dx, dy = Dir.to_offsets dir in
  let x2, y2 = x + dx, y + dy in
  let track1 = get v x y in
  let track2 = get v x2 y2 in
  let track1 = Track.add_dir track1 dir in
  let track2 = Track.add_dir track2 @@ Dir.opposite dir in
  track1, track2, x2, y2

  (* check before building *)
let check_build_track v ~x ~y ~dir ~player =
  let track1, track2, _, _ = pre_build_track v ~x ~y ~dir ~player in
  if Track.is_legal track1 && Track.is_legal track2 then
    `Ok
  else
    `Illegal

    (* do build *)
let build_track v ~x ~y ~dir ~player =
  let track1, track2, x2, y2 = pre_build_track v ~x ~y ~dir ~player in
  if Track.is_legal track1 && Track.is_legal track2 then (
    set v x y track1;
    set v x2 y2 track2;
  )


