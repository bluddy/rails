(* open Containers *)

(* Everything related to the screen *)

type view =
  | Title
  | MainMenu
  | Main
  | MapGen of Mapgen.t option
  | Broker
  | Schedule
  | ScheduleRoute
  | Station
  | BuildStation
  | Animation

type t = {
  menu_open: bool;
  view : view;
}
[@@deriving lens]

let make view = {
  menu_open=false;
  view=view;
}

