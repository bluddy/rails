
(* Main modules of game. They don't carry much state between them *)
type t =
  | MapView
  | MapGen of Mapgen.t option
