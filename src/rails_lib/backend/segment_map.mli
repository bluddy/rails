open! Utils
module TS = Trackmap.Search

type id [@@deriving yojson, eq, show]

type t [@@deriving yojson]
val make : unit -> t
val new_id : t -> id
val incr_train: (loc * Dir.t) -> t -> unit
val decr_train: (loc * Dir.t) -> t -> unit
val reset: id -> t -> unit

val build_track_join_segments: Track_graph.t -> Trackmap.t -> t -> TS.scan -> TS.scan -> t
val remove_track_split_segment: Track_graph.t -> Trackmap.t -> t -> TS.scan -> TS.scan -> t
val build_station_get_segments: Track_graph.t -> t -> Trackmap.t -> Track_graph.Node.t -> TS.scan -> t

