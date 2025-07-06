
(* Main modules of game. They don't carry much state between them *)
type t =
  | GameCreation of Game_creation.t
  | MapGen of Mapgen.t option
  | MapView
