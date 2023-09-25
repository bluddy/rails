open! Utils
module TS = Trackmap.Search

type id [@@deriving yojson, eq, show]

type t [@@deriving yojson]
val make : unit -> t
val new_id : t -> id
val incr_train: (loc * Dir.t) -> t -> unit
val decr_train: (loc * Dir.t) -> t -> unit
val reset: id -> t -> unit

val build_track: Track_graph.t -> Trackmap.t -> t -> TS.scan -> TS.scan -> t
val remove_track: Track_graph.t -> Trackmap.t -> t -> TS.scan -> TS.scan -> t
val remove_station: Track_graph.t -> Trackmap.t -> t -> loc -> TS.scan -> t
val build_station: Track_graph.t -> t -> Trackmap.t -> Track_graph.Node.t -> TS.scan -> t

