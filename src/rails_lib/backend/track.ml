open Containers
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type double_track = [ `Double | `Single ] [@@deriving eq, yojson, hash]

type kind =
  | Track of double_track
  | Ferry of double_track
  | Tunnel
  | Station of Station.kind
  | Bridge of Bridge.t
  [@@deriving eq, hash, yojson]

type t =
  {
    dirs: Dir.Set.t;
    kind: kind;
    ixn: bool;
    player: int;
  } [@@deriving yojson]

let empty player kind = {
  dirs=Dir.Set.empty;
  kind;
  ixn=false;
  player
}

let _is_ixn dirs = Dir.Set.cardinal dirs > 2

let is_empty v = Dir.Set.cardinal v.dirs = 0

let is_station v = match v.kind with
  | Station _ -> true | _ -> false

let is_big_station v = match v.kind with
  | Station station -> Station.is_big_station station
  | _ -> false

let is_ixn v = v.ixn

let make dirs kind ~player =
  let ixn = _is_ixn dirs in
  {dirs; kind; ixn; player}

let add_dir v ~dir =
  let dirs = Dir.Set.add v.dirs dir in
  let ixn = _is_ixn dirs in
  {v with dirs; ixn}

let remove_dir v ~dir =
  let dirs = Dir.Set.remove v.dirs dir in
  let ixn = _is_ixn dirs in
  {v with dirs; ixn}

let has_dir v ~dir =
  Dir.Set.mem v.dirs dir

let equal_dirs v1 v2 =
  Dir.Set.equal v1.dirs v2.dirs

let change_kind v ~kind = {v with kind}

  (* Make hashtabl that only uses track dirs and kind for quick texture lookup *)
module Htbl = Hashtbl.Make(struct
  type nonrec t = t
  let equal x y =
    Dir.Set.equal x.dirs y.dirs && equal_kind x.kind y.kind
  let hash x =
    Dir.Set.to_int x.dirs lxor hash_kind x.kind
end)

  (* Hash-based set for quick checking of legal dirs for Track *)
module TrackSet = CCHashSet.Make(struct
  type t = Dir.Set.t
  let equal = Dir.Set.equal
  let hash = Dir.Set.to_int
end)

let _add_to_set h li =
  List.iter (fun l ->
    let set = Dir.Set.of_list l in
    TrackSet.insert h set
  )
  li

  (* Set of all sets of legal tracks *)
let legal_tracks =
  let h = TrackSet.create 30 in
  _add_to_set h Resources.track_dirs;
  _add_to_set h Resources.track_turns;
  h

  (* Set of all sets of non-track things. Full straight *)
let straight_dirs =
  let h = TrackSet.create 10 in
  _add_to_set h Resources.straight_track;
  h

 (* Set of tracks that can be doubled *) 
let undoubleable_dirs =
  let h = TrackSet.create 10 in
  _add_to_set h Resources.track_turns;
  h

let is_legal_dirs dirs = TrackSet.mem legal_tracks dirs

let is_legal v =
  match v.kind with
  | Track _ | Ferry _ -> TrackSet.mem legal_tracks v.dirs
  | _ -> TrackSet.mem straight_dirs v.dirs

let is_straight v =
  (* Check for straight track of all sorts *)
  TrackSet.mem straight_dirs v.dirs || (Dir.Set.cardinal v.dirs = 1)

  (* Convert a single dir to a straight track *)
let straighten v =
  if Dir.Set.cardinal v.dirs = 1 then
    let dir = Dir.Set.find v.dirs (fun _ -> true) in
    let opposite = Dir.opposite dir in
    let dirs = Dir.Set.add v.dirs opposite in
    {v with dirs}
  else
    if TrackSet.mem straight_dirs v.dirs then v
    else
      failwith "Unexpected non-straight track"

let is_doubleable v =
  match v.kind with
  | Track _
  | Ferry _ when not @@ TrackSet.mem undoubleable_dirs v.dirs -> true
  | _ -> false

let change_to_double v double =
  let kind = match v.kind with
    | Track _ when double -> Track `Double
    | Track _ -> Track `Single
    | Ferry _ when double -> Ferry `Double
    | Ferry _ -> Ferry `Single
    | _ -> v.kind
  in
  [%up {v with kind}]

let is_visually_double v =
  (* Tracks that look double i.e. that require doubling in graphics *)
  match v.kind with
  | Track `Double
  | Ferry `Double -> true
  | _ -> false

let acts_like_double v =
  (* Some tracks aren't double per se, but they act that way *)
  match v.kind with
  | Track `Double
  | Ferry `Double
  | Station _ 
  | Bridge Iron
  | Bridge Stone -> true
  | _ -> false

let double_track_offsets v =
  (* A funky algorithm in the original code for determining the offsets of 
     double tracks
     returns the offsets in pixels for each side
   *)
  let x_offset, y_offset, _ =
    List.fold_left (fun ((x_offset, y_offset, mult) as acc) dir ->
      if Dir.Set.mem v.dirs dir then
        let dir90 = dir |> Dir.cw |> Dir.cw in
        let offx, offy = Dir.to_offsets dir90 in
        (x_offset + offx * mult, y_offset + offy * mult, -mult)
      else acc)
    (0, 0, -1)
    Dir.dirlist_left
  in
  (* Printf.printf "x_off:%d, y_off:%d\n%!" x_offset y_offset; *)
  if abs(x_offset) + abs(y_offset) = 4 then (
    (-x_offset - 1, -y_offset), (x_offset/2 - 1, y_offset/2)
  ) else
    (-x_offset, -y_offset), (x_offset, y_offset)

