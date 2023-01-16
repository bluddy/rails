open Containers

let src = Logs.Src.create "track_graph" ~doc:"The track graph"
module Log = (val Logs.src_log src: Logs.LOG)

(* Graph for intersections and stations *)

(* NOTE: must be per player! *)

module Edge = struct
  type xy_dir = (int * int * Dir.t)
  [@@ deriving yojson]

  type t = {
    id: int;
    nodes: xy_dir * xy_dir;
    dist: int;
  } [@@ deriving yojson]

  let default = {id=0; nodes=((0,0,Dir.Up),(0,0,Dir.Up)); dist=0}
  let equal x y = x.id = y.id
  let compare x y = x.id - y.id
  let make id x1 y1 dir1 x2 y2 dir2 dist =
    let nodes = ((x1,y1,dir1),(x2,y2,dir2)) in
    {id; nodes; dist}

  let eq_xydir x1 y1 dir1 (x2,y2,dir2) =
    x1 = x2 && y1 = y2 && Dir.equal dir1 dir2

    (* Either node can match *)
  let has_xydir x y dir v =
    let d1, d2 = v.nodes in
    eq_xydir x y dir d1 || eq_xydir x y dir d2

    (* Return matching dir for ixn x, y *)
  let dir_of_xy x y v = match v.nodes with
    | ((x1,y1,dir1),_) when x=x1 && y=y1 -> Some dir1
    | (_,(x2,y2,dir2)) when x=x2 && y=y2 -> Some dir2
    | _ -> None
end

module Node = struct
  type t = int * int
    [@@deriving eq, ord, yojson]

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

module Weight = struct
  type edge = Node.t * Edge.t * Node.t
  type t = int

  (* Allows us to block computation of distance for a certain edge *)
  let block_id = ref None

  let set_block_id i = block_id := Some i
  let clear_block_id () = block_id := None
  let weight (_,e,_) =
    match !block_id with
    | Some id when id = e.Edge.id -> 100000
    | _ -> e.dist
  let compare = Int.compare
  let add = (+)
  let zero = 0
end


module ShortestPath = Graph.Path.Dijkstra(G)(Weight)

type t = {
  mutable last_id: int;
  graph: G.t;
} [@@deriving yojson]

let make () = {
  last_id=0;
  graph=G.create ();
}

let add_ixn v ~x ~y =
  Log.debug (fun f -> f "Adding ixn at (%d,%d)" x y);
  G.add_vertex v.graph (x, y);
  v

let remove_ixn v ~x ~y =
  Log.debug (fun f -> f "Removing ixn at (%d,%d)" x y);
  G.remove_vertex v.graph (x, y);
  v

let add_segment v ~x1 ~y1 ~dir1 ~x2 ~y2 ~dir2 ~dist =
  Log.debug (fun f -> f "Adding segment from (%d,%d,%s) to (%d,%d,%s), dist %d"
              x1 y1 (Dir.show dir1) x2 y2 (Dir.show dir2) dist);
  let id = v.last_id in
  v.last_id <- succ v.last_id;
  let edge = Edge.make id x1 y1 dir1 x2 y2 dir2 dist in
  G.add_edge_e v.graph ((x1,y1),edge,(x2,y2));
  v

let remove_segment v ~x ~y ~dir =
  Log.debug (fun f -> f "Remove segment from (%d,%d,%s)" x y (Dir.show dir));
  (* Find the edge we want *)
  let edge =
    G.fold_succ_e (fun ((_,e,_) as edge) acc ->
      if Edge.has_xydir x y dir e then Some edge
      else acc
    ) v.graph (x,y) None
  in
  match edge with
  | None -> v 
  | Some edge ->
      G.remove_edge_e v.graph edge;
      v

let fold_succ_ixns f v ~ixn ~init = G.fold_succ f v.graph ixn init
let iter_succ_ixns f v ~ixn = G.iter_succ f v.graph ixn

  (* Find the shortest path branch from an ixn given a from_dir entering the ixn,
     which is excluded *)
let shortest_path_branch ~ixn ~dir ~dest v =
  (* First get dir_from and disable it *)
  let x, y = ixn in
  let rev_dir = Dir.opposite dir in
  G.iter_succ_e (fun (_,e,_) ->
    if Edge.has_xydir x y rev_dir e then Weight.set_block_id e.id)
    v.graph ixn;
  let path, _ = ShortestPath.shortest_path v.graph ixn dest in
  Weight.clear_block_id ();
  match path with
  | (_,edge,_)::_ -> Edge.dir_of_xy x y edge
  | _ -> None


