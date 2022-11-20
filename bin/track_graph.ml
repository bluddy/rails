open Containers

(* Graph for intersections and stations *)

module Node = struct
  type t = {
    x: int;
    y: int;
    is_station: bool;
  } [@@deriving eq, ord]
  let hash = Hashtbl.hash

end

module Edge = struct
  type t = {
    dist: int;
  } [@@ deriving ord]
  let default = {dist=0}
end

module G = Graph.Imperative.Graph.ConcreteLabeled(Node)(Edge)

type t = G.t

  





