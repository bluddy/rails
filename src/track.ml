open Containers

type kind =
  | Track
  | Tunnel
  | Station of Station.t
  | WoodBridge
  | MetalBridge
  | StoneBridge
  [@@deriving eq, hash]

type t =
  {
    dirs: Dir.Set.t;
    kind: kind;
    ixn: bool;
    player: int;
  }

let empty player = {
  dirs=Dir.Set.empty;
  kind=Track;
  ixn=false;
  player
}

let is_ixn dirs = Dir.Set.cardinal dirs > 2

let make dirs kind ~player =
  let ixn = is_ixn dirs in
  {dirs; kind; ixn; player}

let add_dir v dir =
  let dirs = Dir.Set.add v.dirs dir in
  let ixn = is_ixn dirs in
  {v with dirs; ixn}

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

  (* Set of all sets of non-track things *)
let straight_dirs =
  let h = TrackSet.create 10 in
  _add_to_set h Resources.special_dirs;
  h

let is_legal_dirs dirs = TrackSet.mem legal_tracks dirs

let is_legal v =
  match v.kind with
  | Track -> TrackSet.mem legal_tracks v.dirs
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




