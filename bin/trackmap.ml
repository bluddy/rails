open Containers

(* This map contains independent tiles of track *)

type t = {
  map: (int, Track.t) Utils.Hashtbl.t;
  width: int;
  height: int;
} [@@deriving yojson]

let empty width height =
  let map = Hashtbl.create 100 in
  {map; width; height}

let get v x y = Hashtbl.find_opt v.map (Utils.calc_offset v.width x y)

  (* get, buf if there's nothing, create a track *)
let get_track_default ?(kind=Track.Track) v x y ~player =
  get v x y
  |> Option.get_lazy (fun () -> Track.empty player kind)

let set v x y tile = Hashtbl.replace v.map (Utils.calc_offset v.width x y) tile

let remove v x y = Hashtbl.remove v.map (Utils.calc_offset v.width x y)

let iter v f =
  Hashtbl.iter (fun i track ->
    let x, y = Utils.x_y_of_offset v.width i in
    f x y track)
  v.map

let out_of_bounds v x y =
  x < 0 || y < 0 || x >= v.width || y >= v.height

  (* Common function for moving by dir *)
let move_dir v ~x ~y ~dir =
  let dx, dy = Dir.to_offsets dir in
  let x2, y2 = x + dx, y + dy in
  if out_of_bounds v x2 y2 then None
  else Some(x2, y2)

let check_build_track v ~x ~y ~dir ~player =
  match out_of_bounds v x y, move_dir v ~x ~y ~dir with
  | false, _ | _, None  -> false
  | _, Some (x2, y2) ->
    let track1 = get_track_default v x y ~player in
    let track2 = get_track_default v x2 y2 ~player in
    let track12 = Track.add_dir track1 ~dir in
    let track22 = Track.add_dir track2 ~dir:(Dir.opposite dir) in
    if Track.equal_dirs track1 track12 && Track.equal_dirs track2 track22 then false
    else
      track1.player = player && track2.player = player
      && Track.is_legal track12 && Track.is_legal track22

  (* do build. Assumes we already checked
     kind1: kind of track in from tile
     kind2: kind of track in to tile
   *)
let build_track ?kind1 ?kind2 v ~x ~y ~dir ~player =
  match move_dir v ~x ~y ~dir with
  | None -> v
  | Some (x2, y2) ->
    let track1 = get_track_default ?kind:kind1 v x y ~player in
    let track2 = get_track_default ?kind:kind2 v x2 y2 ~player in
    let track1 = Track.add_dir track1 ~dir in
    let track2 = Track.add_dir track2 ~dir:(Dir.opposite dir) in
    set v x y track1;
    set v x2 y2 track2;
    v

 let check_build_station v ~x ~y ~player station_type =
   if out_of_bounds v x y then `Illegal
   else match get v x y with
   | None -> `NoTrack
   | Some ({kind=Track;_} as t) when t.player = player && Track.is_straight t ->
        let range = Station.to_range station_type in
        let match_fn j i =
          match get v j i with
          | Some {kind=Station(st);_} ->
              let range2 = Station.to_range st in
              let range = range + range2 in
              abs (j - x) < range && abs (i - y) < range
          | _ -> false
        in
        let station_test =
          Utils.scan ~range ~x ~y ~width:v.width ~height:v.height ~f:match_fn
        in
        begin match station_test with
        | Some _ -> `TooClose
        | None -> `Ok
        end
   | _ -> `Illegal
   
let build_station v ~x ~y station_type =
  match get v x y with
  | Some ({kind=Track; _} as t) ->
      (* Do we build new track *)
      let build_new_track = Dir.Set.cardinal t.dirs > 1 in
      let track = Track.straighten t in
      let station = {track with kind=Station(station_type)} in
      set v x y station;
      v, build_new_track
  | _ -> assert false

  (* Check that a stretch of track is clear: tunnel or bridge
     No track can be in the middle, but it's ok if track is at the end
     Length: includes middle stretch + last piece
   *)
let check_build_stretch v ~x ~y ~dir ~player ~length =
  let dx, dy = Dir.to_offsets dir in
  let x1, y1 = x, y in
  let x3, y3 = x + dx * length, y + dy * length in
  (* check boundaries *)
  if x < 0 || x3 < 0 || y < 0 || y3 < 0 ||
    x >= v.width || x3 >= v.width || y >= v.height || y3 >= v.height then
      false
  else (
    (* No track in between *)
    let rec loop x y i =
      if i <= 0 then true
      else
        match get v x y with
        | Some _ -> false
        | None ->
            loop (x+dx) (y+dy) (i-1)
    in

    let test_track x y dir =
      match get v x y with
      | Some track when track.player = player ->
          let track = Track.add_dir track ~dir in
          Track.is_legal track
      | Some _ ->
          (* wrong player *)
          false
      | _ ->
          (* no track = fine *)
          true
    in
    test_track x1 y1 dir &&
    loop (x1+dx) (y1+dy) (length-1) &&
    test_track x3 y3 (Dir.opposite dir)
  )

    (* Used by bridge and tunnel *)
let build_stretch v ~x ~y ~dir ~player ~n ~kind =
  let dx, dy = Dir.to_offsets dir in
  let x1, y1 = x, y in
  let x3, y3 = x + dx * n, y + dy * n in
  if x < 0 || x3 < 0 || y < 0 || y3 < 0 ||
    x >= v.width || x3 >= v.width || y >= v.height || y3 >= v.height then
      v (* error *)
  else (
    let track1 = get_track_default v x1 y1 ~player |> Track.add_dir ~dir in
    set v x1 y1 track1;
    let track2 = Track.empty player kind
      |> Track.add_dir ~dir
      |> Track.add_dir ~dir:(Dir.opposite dir)
    in
    let rec dig_tunnel ~x ~y i =
      if i <= 0 then ()
      else (
        set v x y track2;
        dig_tunnel ~x:(x+dx) ~y:(y+dy) (i-1)
      )
    in
    dig_tunnel ~x:(x1+dx) ~y:(y1+dy) (n-1);
    let track3 = get_track_default v x3 y3 ~player |> Track.add_dir ~dir:(Dir.opposite dir) in
    set v x3 y3 track3;
    v
  )

let build_bridge v ~x ~y ~dir ~player ~kind =
  build_stretch v ~x ~y ~dir ~player ~n:2 ~kind:(Track.Bridge kind)

let build_tunnel v ~x ~y ~dir ~player ~length =
  build_stretch v ~x ~y ~dir ~player ~n:length ~kind:Track.Tunnel
   
  (* Can work for all kinds of constructs *)
let check_remove_track v ~x ~y ~dir ~player =
  match out_of_bounds v x y, move_dir v ~x ~y ~dir with
  | false, _ | _, None  -> false
  | _, Some (x2, y2) ->
    let track1 = get_track_default v x y ~player in
    let track2 = get_track_default v x2 y2 ~player in
    if track1.player <> player || track2.player <> player then false
    else
      match track1.kind, track2.kind with
      | Bridge _, _ | _, Bridge _ -> true
      | Tunnel, _ | _, Tunnel -> true
      | Station _, _ | _, Station _ -> true
      | _ ->
          Track.has_dir track1 ~dir || Track.has_dir track2 ~dir:(Dir.opposite dir)

let remove_track v ~x ~y ~dir ~player =
  let remove_track_dir v ~x ~y ~dir =
    match get v x y with
    | Some track ->
      let track = Track.remove_dir track ~dir in
      if Track.is_empty track then
        remove v x y
      else
        set v x y track
    | None -> ()
  in
  let dx, dy = Dir.to_offsets dir in
  let x2, y2 = x + dx, y + dy in
  let track1 = get_track_default v x y ~player in
  if track1.player = player then
    begin match track1.kind with
    | Track | Ferry ->
        (* TODO: handle ixn *)
        remove_track_dir v ~x ~y ~dir;
    | Station _ ->
        (* TODO: handle station removal *)
        remove v x y
    | Bridge _ | Tunnel ->
        remove v x y
    end
  else ();
  let track2 = get_track_default v x2 y2 ~player in
  begin match track2.kind with
  | Track | Ferry when track2.player = player ->
      remove_track_dir v ~x:x2 ~y:y2 ~dir:(Dir.opposite dir);
  | _ ->
      (* All other constructs remain whole with full dirs *)
      ()
  end;
  v

  (* Scan for a new segment ending in a station or ixn *)
let _scan_segment v ~target_x ~target_y ~dir ~player =
  (* explore along dir *)
  let player2 = player in
  let rec loop_to_node x y dir dist =
    match get v x y with
    | Some {ixn = true; player; _} when player = player2 ->
        Some (x,y,dist,`Ixn)
    | Some {kind = Station _; player; _} when player = player2 ->
        Some (x,y,dist,`Station)
    | Some {dirs; player; _} when player = player2 ->
        (* Find other dir and follow it *)
        let other_dirs = Dir.Set.remove dirs (Dir.opposite dir) in
        begin match Dir.Set.pop_opt other_dirs with
        | Some (other_dir, _) ->
          begin match move_dir v ~x ~y ~dir:other_dir with
          | None -> None
          | Some (x2, y2) ->
              loop_to_node x2 y2 other_dir (dist+1)
          end
        | None -> None
        end
    | _ -> None
  in
  loop_to_node target_x target_y dir 0
        
