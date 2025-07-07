
(* Main modules of game. They don't carry much state between them *)
type t =
  | MainMenu of Main_menu.t
  | MapGen of Mapgen.t option
  | MapView
