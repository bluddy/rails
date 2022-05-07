open Containers

type t = {
  map: (int, Track.t) Hashtbl.t;
  count: int;
  width: int;
  height: int;
}

let empty width height =
  let map = Hashtbl.create 100 in
  let count = 0 in
  {map; width; height; count}

let calc_offset v x y = y * v.width + x

let x_y_of_offset v offset =
  let y = offset / v.width in
  let x = offset mod v.width in
  x, y

let get v x y = Hashtbl.find_opt v.map (calc_offset v x y)

let get_track v x y ~player =
  get v x y
  |> Option.get_lazy (fun () -> Track.empty player)

let set v x y tile = Hashtbl.replace v.map (calc_offset v x y) tile

let iter v f =
  Hashtbl.iter (fun i track ->
    let x, y = x_y_of_offset v i in
    f x y track)
  v.map

let pre_build_track v ~x ~y ~dir ~player =
  let dx, dy = Dir.to_offsets dir in
  let x2 = x + dx |> Utils.clip ~min:0 ~max:(v.width - 1) in
  let y2 = y + dy |> Utils.clip ~min:0 ~max:(v.height - 1) in
  let track1 = get_track v x y ~player in
  let track2 = get_track v x2 y2 ~player in
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

 let check_build_station v ~x ~y ~player station_type =
   match get v x y with
   | None -> `NoTrack
   | Some ({kind=Track;_} as t) when t.player = player && Track.is_straight t ->
         let range = Station.range_of station_type in
         let match_fn j i =
           match get v j i with
           | Some {kind=Station(st);_} ->
               let range2 = Station.range_of st in
               let range = range + range2 in
               abs (j - x) < range && abs (i - y) < range
           | _ -> false
         in
         let station_test =
           Utils.scan ~range ~x ~y ~max_x:(v.width - 1) ~max_y:(v.height-1) ~f:match_fn
         in
         if station_test then `TooClose
         else `Ok

   | _ -> `Illegal
   



  




