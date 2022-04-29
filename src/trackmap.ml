open Containers

type t = {
  map: (int, Track.t) Hashtbl.t;
  count: int;
  w: int;
  h: int;
}

let empty w h =
  let map = Hashtbl.create 100 in
  let count = 0 in
  {map; w; h; count}

let calc_offset v x y = y * v.w + x

let x_y_of_offset v offset =
  let y = offset / v.w in
  let x = offset mod v.w in
  x, y

let get v x y = Hashtbl.find_opt v.map (calc_offset v x y)

let set v x y tile = Hashtbl.replace v.map (calc_offset v x y) tile

let iter v f =
  Hashtbl.iter (fun i track ->
    let x, y = x_y_of_offset v i in
    f x y track)
  v.map

let pre_build_track v ~x ~y ~dir ~player =
  let dx, dy = Dir.to_offsets dir in
  let x2 = x + dx |> Utils.clip ~min:0 ~max:(v.w - 1) in
  let y2 = y + dy |> Utils.clip ~min:0 ~max:(v.h - 1) in
  let track1 = get v x y
    |> Option.get_lazy (fun () -> Track.empty player) in
  let track2 = get v x2 y2
    |> Option.get_lazy (fun () -> Track.empty player) in
  let track1 = Track.add_dir track1 dir in
  let track2 = Track.add_dir track2 @@ Dir.opposite dir in
  track1, track2, x2, y2

  (* check before building *)
let check_build_track v ~x ~y ~dir ~player =
  let track1, track2, x2, y2 = pre_build_track v ~x ~y ~dir ~player in
  (* Check for out of bounds: x and y didn't change *)
  if (x <> x2 || y <> y2) &&
     Track.is_legal track1 && Track.is_legal track2 then
    `Ok
  else
    `Illegal

    (* do build *)
let build_track v ~x ~y ~dir ~player =
  let track1, track2, x2, y2 = pre_build_track v ~x ~y ~dir ~player in
  if Track.is_legal track1 && Track.is_legal track2 then (
    set v x y track1;
    set v x2 y2 track2;
    { v with count = v.count + 1}
  )
  else v


