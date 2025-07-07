
(* Main modules of game. They don't carry much state between them *)
type 'state t =
  | MainMenu of 'state Main_menu_d.t
  | MapGen of Mapgen.t option
  | MapView
