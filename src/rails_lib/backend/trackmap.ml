open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Utils

(* This map contains independent tiles of track *)
type t = {
  map: Track.t IntMap.t;
  width: int;
  height: int;
} [@@deriving yojson]

let empty width height =
  let map = IntMap.empty in
  {map; width; height}

let get_xy x y v = IntMap.find_opt (Utils.calc_offset v.width x y) v.map
let get (x,y) v = get_xy x y v

let get_xy_exn x y v = IntMap.find (Utils.calc_offset v.width x y) v.map 
let get_exn (x,y) v = get_xy_exn x y v

  (* get, buf if there's nothing, create a track *)
let get_track_default ?(kind=(Track.Track `Single)) loc player_idx v =
  get loc v
  |> Option.get_lazy (fun () -> Track.empty player_idx kind)

let set_xy x y t v =
  let map = IntMap.add (Utils.calc_offset v.width x y) t v.map in
  {v with map}
let set (x,y) t v = set_xy x y t v

let update_loc (x, y) f v =
  {v with map=IntMap.update (Utils.calc_offset v.width x y) f v.map}

let remove_xy x y v =
  let map = IntMap.remove (Utils.calc_offset v.width x y) v.map in
  {v with map}
let remove (x, y) v = remove_xy x y v

let iter v f =
  IntMap.iter (fun i track ->
    let x, y = Utils.x_y_of_offset v.width i in
    f x y track)
  v.map

let fold f v ~init =
  IntMap.fold (fun i track acc ->
    let loc = Utils.x_y_of_offset v.width i in
    f loc track acc)
  v.map
  init

let out_of_bounds_xy x y v = x < 0 || y < 0 || x >= v.width || y >= v.height
let out_of_bounds (x, y) v = out_of_bounds_xy x y v

(* Common function for moving by dir, with bounds check *)
let move_dir_bounds loc ~dir v =
  let loc2 = Dir.adjust_loc dir loc in
  if out_of_bounds loc2 v then None
  else Some loc2

let check_build_track loc ~dir player_idx v =
  match out_of_bounds loc v, move_dir_bounds loc ~dir v with
  | true, _ | _, None  -> false
  | _, Some loc2 ->
    let track1 = get_track_default loc player_idx v in
    let track2 = get_track_default loc2 player_idx v in
    let track12 = Track.add_dir track1 ~dir in
    let track22 = Track.add_dir track2 ~dir:(Dir.opposite dir) in
    (* Check that we changed something *)
    if Track.equal_dirs track1 track12 && Track.equal_dirs track2 track22 then false
    else
      Owner.(track1.player = player_idx && track2.player = player_idx)
      && Track.is_legal track12 && Track.is_legal track22

  (* do build. Assumes we already checked
     kind1: kind of track in from tile
     kind2: kind of track in to tile
   *)
let build_track ?kind1 ?kind2 loc ~dir player_idx v =
  match move_dir_bounds loc ~dir v with
  | None -> v
  | Some loc2 ->
    let track1 = get_track_default ?kind:kind1 loc player_idx v in
    let track2 = get_track_default ?kind:kind2 loc2 player_idx v in
    let track1 = Track.add_dir track1 ~dir in
    let track2 = Track.add_dir track2 ~dir:(Dir.opposite dir) in
    let v = set loc track1 v in
    let v = set loc2 track2 v in
    v

let check_build_station ((x, y) as loc) player_idx station_type v =
  if out_of_bounds loc v then `Illegal
  else match get loc v with
  | None -> `NoTrack
  | Some ({kind=Track _;_} as t) when Owner.(t.player = player_idx) && Track.is_straight t ->
       let range = Station.to_range station_type in
       let match_fn j i = match get_xy j i v with
         | Some {kind=Station(st);_} ->
             let range2 = Station.to_range st in
             let range = range + range2 in
             abs (j - x) < range && abs (i - y) < range
         | _ -> false
       in
       let station_test = Utils.scan x y ~range ~width:v.width ~height:v.height ~f:match_fn in
       if Option.is_some station_test then `TooClose else `Ok
  | _ -> `Illegal
   
let build_station v loc station_type =
  match get loc v with
  | Some ({kind=Track _; _} as t) ->
      (* Do we build new track *)
      let build_new_track_dir =
        if Dir.Set.cardinal t.dirs <= 1 then
          Some(Dir.Set.pop t.dirs |> fst |> Dir.opposite) else None
      in
      let track = Track.straighten t in
      let station = {track with kind=Station(station_type)} in
      let v = set loc station v in
      v, build_new_track_dir
  | _ -> assert false

  (* Check that a stretch of track is clear: tunnel or bridge
     No track can be in the middle, but it's ok if track is at the end
     Length: includes middle stretch + last piece
   *)
let check_build_stretch ((x, y) as loc) ~dir player_idx ~length v =
  let dx, dy = Dir.to_offsets dir in
  let (x1, y1) as loc1 = loc in
  let loc3 = x + dx * length, y + dy * length in
  (* check boundaries *)
  if out_of_bounds loc v || out_of_bounds loc3 v then false
  else (
    (* No track in between *)
    let rec loop x y ~n =
      if n <= 0 then true
      else
        match get_xy x y v with
        | Some _ -> false
        | None ->
            loop (x+dx) (y+dy) ~n:(n-1)
    in
    let test_track loc dir =
      match get loc v with
      | Some track when Owner.(track.player = player_idx) ->
          let track = Track.add_dir track ~dir in
          Track.is_legal track
      | Some _ ->
          (* wrong player *)
          false
      | _ ->
          (* no track = fine *)
          true
    in
    test_track loc1 dir &&
    loop (x1 + dx) (y1 + dy) ~n:(length-1) &&
    test_track loc3 (Dir.opposite dir)
  )

    (* Used by bridge and tunnel *)
let build_stretch ((x, y) as loc) ~dir player_idx ~n ~kind v =
  let dx, dy = Dir.to_offsets dir in
  let (x1, y1) as loc1 = loc in
  let loc3 = x + dx * n, y + dy * n in
  if out_of_bounds loc v || out_of_bounds loc3 v then v (* error *)
  else (
    let track1 = get_track_default loc1 player_idx v
      |> Track.add_dir ~dir
    in
    let v = set loc1 track1 v in
    let track2 = Track.empty player_idx kind
      |> Track.add_dir ~dir
      |> Track.add_dir ~dir:(Dir.opposite dir)
    in
    let rec dig_tunnel x y ~n v =
      if n <= 0 then v
      else
        let v = set_xy x y track2 v in
        dig_tunnel (x+dx) (y+dy) ~n:(n-1) v
    in
    let v = dig_tunnel (x1+dx) (y1+dy) ~n:(n-1) v in
    let track3 =
      get_track_default loc3 player_idx v
      |> Track.add_dir ~dir:(Dir.opposite dir) in
    let v = set loc3 track3 v in
    v
  )

let build_bridge loc ~dir player_idx ~kind v =
  build_stretch loc ~dir player_idx ~n:2 ~kind:(Track.Bridge kind) v

let build_tunnel loc ~dir player_idx ~length v =
  build_stretch loc ~dir player_idx ~n:length ~kind:Track.Tunnel v
   
  (* Can work for all kinds of constructs *)
let check_remove_track loc ~dir player_idx v =
  match out_of_bounds loc v, move_dir_bounds loc ~dir v with
  | true, _ | _, None  -> false
  | _, Some loc2 ->
    let track1 = get_track_default loc player_idx v in
    let track2 = get_track_default loc2 player_idx v in
    if Owner.(track1.player <> player_idx || track2.player <> player_idx) then false
    else
      match track1.kind, track2.kind with
      | Bridge _, _ | _, Bridge _
      | Tunnel, _ | _, Tunnel
      | Station _, _ | _, Station _ -> true
      | _ ->
          Track.has_dir track1 ~dir || Track.has_dir track2 ~dir:(Dir.opposite dir)

let remove_track loc ~dir player_idx v =
  let remove_track_dir loc ~dir v =
    match get loc v with
    | Some track ->
      let track = Track.remove_dir track ~dir in
      if Track.is_empty track then
        remove loc v
      else
        set loc track v
    | None -> v
  in
  let loc2 = Dir.adjust_loc dir loc in
  let track1 = get_track_default loc player_idx v in
  let v =
    if Owner.(track1.player = player_idx) then
      match track1.kind with
      | Track _ | Ferry _ ->
          (* TODO: handle ixn *)
          remove_track_dir loc ~dir v;
      | Station _ ->
          (* TODO: handle station removal *)
          remove loc v
      | Bridge _ | Tunnel ->
          remove loc v
    else v
  in
  let track2 = get_track_default loc2 player_idx v in
  match track2.kind with
  | Track _ | Ferry _ when Owner.(track2.player = player_idx) ->
      remove_track_dir loc2 ~dir:(Dir.opposite dir) v;
  | _ ->
      (* All other constructs remain whole with full dirs *)
      v

let has_station loc v =
  match get loc v with
  | Some t -> Track.is_station t
  | None -> false

let has_track loc v = Option.is_some @@ get loc v 

let calc_total_dist v ~player =
  IntMap.sum (fun _ track ->
    if Owner.(track.Track.player = player) then
      Track.calc_dist track
    else 0)
  v.map

let calc_total_land_cost v ~player =
  IntMap.sum (fun _ track ->
    if Owner.(track.Track.player = player) then
      Track.calc_dist track
    else 0)
  v.map

let find_ixns_in_range ~x ~y ~range v =
  (* ixns are critical to the track graph *)
  Iter.fold (fun acc i ->
    Iter.fold (fun acc j ->
      if get_xy j i v |> Option.map_or ~default:false Track.is_ixn then (j, i)::acc
      else acc)
    acc
    Iter.((x-range) -- (x+range))
  )
  []
  Iter.((y-range) -- (y+range))

