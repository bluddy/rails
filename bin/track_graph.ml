open Containers

(* Graph for intersections and stations *)

module Node = struct
  type t = {
    x: int;
    y: int;
    is_station: bool;
  } [@@deriving eq, ord, yojson]
  let hash = Hashtbl.hash

end

module Edge = struct
  type t = {
    dist: int;
  } [@@ deriving ord, yojson]
  let default = {dist=0}
end

module G = Graph.Imperative.Graph.ConcreteLabeled(Node)(Edge)

type t = G.t

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

