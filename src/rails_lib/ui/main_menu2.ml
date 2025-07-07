open! Containers

module R = Renderer
module B = Backend
module C = Constants
module M = Money

open Utils.Infix

type menu =
  | Action
  | Difficulty
  | Options

type t =  unit
