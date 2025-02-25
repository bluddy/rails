(* open Containers *)

(* The basic modules of the game, specifically ones that don't need
   to carry heavy info between each other *)

type t =
  | MapView
  | MapGen of Mapgen.t option

  (* TODO:
     Title
     Retirement
  *)

