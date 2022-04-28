open Containers

type t = {
  map: Track.t option array;
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
      let track = Track.make dirs Track.Track ~player in
      set v x y (Some track);
      true
  | Some({kind=Track.Track;_} as t) when t.player = player ->
      let dirs = Dir.Set.add t.dirs dir in
      if Track.is_legal dirs then (
        set v x y @@ Some {t with dirs; ixn=Track.is_ixn dirs};
        true
      ) else
        false
  | _ -> false

