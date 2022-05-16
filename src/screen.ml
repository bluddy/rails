(* open Containers *)

(* Everything related to the screen *)

type t =
  | Title
  | MainMenu
  | MapView
  | MapGen of Mapgen.t option
  | Broker
  | Schedule
  | ScheduleRoute
  | Station
  | BuildStation
  | Animation


