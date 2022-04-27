open Containers

type t = {
  map: t option array;
  w: int;
  h: int;
}

let empty w h =
  let map = Array.make (w * h) None in
  {map; w; h}

let calc_offset v x y = y * v.w + x

let get v x y = v.map.(calc_offset v x y)

let set v x y tile =
  v.map.(calc_offset v x y) <- tile

let build_track v ~x ~y ~dir ~player : bool =
  match get v x y with
  | None ->
      let dirs = Dir.Set.singleton dir in
      let track = make dirs Track player in
      set v x y (Some track);
      true
  | Some({kind=Track;_} as t) when t.player = player ->
      let dirs = Dir.Set.add t.dirs dir in
      if TrackSet.mem legal_tracks dirs then (
        let ixn = Dir.Set.count dirs > 2 in
        set v x y @@ Some {t with dirs; ixn};
        true
      ) else
        false
  | _ -> false

