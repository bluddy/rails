(* open Containers *)

(* Everything related to the screen *)

type view =
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

type t = {
  view : view;
}
[@@deriving lens]

let make view = {
  view; 
}

