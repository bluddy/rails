open! Utils

type id [@@deriving yojson, eq, show]

type t [@@deriving yojson]
val make : unit -> t
val new_id : t -> id
val incr_train: (loc * Dir.t) -> t -> unit
val decr_train: (loc * Dir.t) -> t -> unit
val reset: id -> t -> unit

val build_track: Track_graph.t -> Trackmap.t -> t -> Scan.t -> Scan.t -> t
val remove_track: Track_graph.t -> Trackmap.t -> t -> Scan.t -> Scan.t -> t
val remove_station: Track_graph.t -> Trackmap.t -> t -> loc -> Scan.t -> t
val build_station: Track_graph.t -> t -> Trackmap.t -> Track_graph.Node.t -> Scan.t -> t

