open! Containers

module R = Engine.Renderer

type module_t =
  | Intro
  | StartMenu
  | Investigate
  | Driving
  | Hacking
  | Crypto
  | Break_in

(* All state *)
type t = {
  (* saveable *)
  mode: module_t;

  (* non-saveable *)
  srv: Services.t;
}

