open Containers

(* Graph for intersections and stations *)

module Edge = struct
  type t = {
    id: int; (* unique id *)
    dist: int;
  } [@@ deriving eq, ord, yojson]

  let default = {id=0; dist=0}
end

module Node = struct
  type t = {
    x: int;
    y: int;
    is_station: bool;
  } [@@deriving eq, ord, yojson]
  let hash = Hashtbl.hash
end


module G = struct
  module G = Graph.Imperative.Graph.ConcreteLabeled(Node)(Edge)
  include G

  let yojson_of_t v : Yojson.Safe.t =
    let edges = G.fold_edges_e (fun e li ->
      (`List [Node.yojson_of_t (G.E.src e);
              Node.yojson_of_t (G.E.dst e);
              Edge.yojson_of_t (G.E.label e)])::li)
      v []
    in
    `List edges

  let t_of_yojson = function
    | `List edges ->
        let g = G.create () in
        List.iter (function 
          | `List[v1; v2; edata] ->
              let v1 = Node.t_of_yojson v1 in
              let v2 = Node.t_of_yojson v2 in
              let edge = Edge.t_of_yojson edata in
              G.add_edge_e g (v1, edge, v2)
          | _ -> invalid_arg "JSON: Bad data")
        edges;
        g
    | _ -> invalid_arg "Graph vertex/edge data not found"
end

type ixn_data = {
  node: Node.t;
  edges: (Dir.t * Edge.t) list;
} [@@deriving yojson]

type t = {
  mutable last_id: int;
  graph: G.t;
  ixn_map: ixn_data Loc_map.t;
} [@@deriving yojson]

let make width = {
  last_id=0;
  graph=G.create ();
  ixn_map = Loc_map.create width;
}

let add_ixn g ~x ~y ~is_station =
  let ixn = Node.{x; y; is_station} in
  G.add_vertex g ixn;
  g

let remove_ixn g ixn =
  G.remove_vertex g ixn;
  g

let remove_segment g segment ixn1 ixn2 =
  G.remove_edge_e g (ixn1,segment,ixn2);
  g

let add_segment g ixn1 ixn2 dist =
  let id = g.last_id in
  g.last_id <- g.last_id + 1;
  let edge = Edge.{id; dist} in
  G.add_edge_e g.graph (ixn1,edge,ixn2);
  g

