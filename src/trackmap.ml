open Containers

type t = {
  map: (int, Track.t) Hashtbl.t;
  width: int;
  height: int;
}

let empty width height =
  let map = Hashtbl.create 100 in
  {map; width; height}

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

  (* do build. Assumes we already checked *)
let build_track v ~x ~y ~dir ~player =
  let track1, track2, x2, y2 = pre_build_track v ~x ~y ~dir ~player in
  set v x y track1;
  set v x2 y2 track2;
  v

 let check_build_station v ~x ~y ~player station_type =
   if x < 0 || x >= v.width || y < 0 || y >= v.width then `Illegal
   else match get v x y with
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
   
let build_station v ~x ~y station_type =
  match get v x y with
  | Some ({kind=Track;_} as t) ->
      let track = Track.straighten t in
      let station = {track with kind=Station(station_type)} in
      set v x y station;
      v
  | _ -> v

let check_build_bridge v ~x ~y ~dir ~player =
  let dx, dy = Dir.to_offsets dir in
  let x2, y2 = x + dx, y + dy in
  let x3, y3 = x2 + dx, y2 + dy in
  if x < 0 || x3 < 0 || y < 0 || y3 < 0 ||
    x >= v.width || x3 >= v.width || y >= v.height || y3 >= v.height then
      `Illegal
  else
    (* Must have some track *)
    match get v x y, get v x2 y2, get_track v x3 y3 player with
    | _, Some track2, _ -> `Illegal (* Bridge already exists *)
    | Some track1, None, ({kind=Track; _} as track3) ->
        let track1 = Track.add_dir track1 dir in
        let track3 = Track.add_dir track3 ~dir:(Dir.opposite dir) in
        if Track.is_legal track1 && Track.is_legal track3 then `Ok
        else `Illegal
    | _ -> `Illegal

let build_bridge v ~x ~y ~dir ~player ~kind =
  let kind = match kind with
    `Wood -> Track.WoodBridge | `Stone -> Track.StoneBridge | `Metal -> Track.MetalBridge
  in
  let dx, dy = Dir.to_offsets dir in
  let x2, y2 = x + dx, y + dy in
  let x3, y3 = x2 + dx, y2 + dy in
  if x < 0 || x3 < 0 || y < 0 || y3 < 0 ||
    x >= v.width || x3 >= v.width || y >= v.height || y3 >= v.height then v
  else
    begin match get v x y with
    | Some track1 ->
        let track1 = Track.add_dir track1 ~dir in
        let track2 =
          Track.empty player
          |> Track.add_dir ~dir
          |> Track.add_dir ~dir:(Dir.opposite dir)
          |> Track.change_kind ~kind
        in
        let track3 =
          Track.empty player
          |> Track.add_dir ~dir:(Dir.opposite dir)
        in
        set v x y track1;
        set v x2 y2 track2;
        set v x3 y3 track3;
        v
    | None -> v (* error *)
    end
   


