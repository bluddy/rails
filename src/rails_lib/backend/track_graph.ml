open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Utils

let src = Logs.Src.create "track_graph" ~doc:"The track graph"
module Log = (val Logs.src_log src: Logs.LOG)

(* Graph for intersections and stations
   Navigation must use both intersections and stations
 *)

(* NOTE: must be per player! *)

module Edge : sig 
    type t = private {
      nodes: locdpair;
      dist: int;
      mutable block: bool;
    } [@@deriving yojson]

    val default: t
    val equal : t -> t -> bool
    val compare: t -> t -> int
    val make: int -> int -> Dir.t -> int -> int -> Dir.t -> int -> t
    val has_xydir: int -> int -> Dir.t -> t -> bool
    val dir_of_xy: loc -> t -> Dir.t option
    val set_block: bool -> t -> unit
  end
= struct
    type t = {
      nodes: locdpair;
      dist: int; (* Length of edge *)
      mutable block: bool; (* Temporarily block the edge *)
    } [@@deriving yojson]

    let canonical v =
      let nodes = canonical_locdpair v.nodes in 
      [%up {v with nodes}]

    (* Assume we're operating on canonical values *)
    let equal x y = equal_locdpair x.nodes y.nodes

    let compare x y = compare_locdpair x.nodes y.nodes

    (* We always make sure we're canonical *)
    let make x1 y1 dir1 x2 y2 dir2 dist =
      let nodes = ((x1,y1),dir1), ((x2,y2),dir2) in
      canonical {nodes; dist; block=false}

    let default = make 0 0 Dir.Up 0 0 Dir.Up 0

      (* Either node can match *)
    let has_xydir x y dir v =
      let d1, d2 = v.nodes in
      equal_locd ((x,y),dir) d1 || equal_locd ((x,y),dir) d2

      (* Return matching dir for ixn x, y *)
    let dir_of_xy (x,y) v = match v.nodes with
      | ((x1,y1),dir1),_ when x=x1 && y=y1 -> Some dir1
      | _,((x2,y2),dir2) when x=x2 && y=y2 -> Some dir2
      | _ -> None

    let set_block b v = v.block <- b
end

module Node = struct
  type t = loc
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

  (* Used for path calculation. Consider blocked edges *)
  let weight (_,e,_) =
    if e.Edge.block then 100000
    else e.dist
  let compare = Int.compare
  let add = (+)
  let zero = 0
end

(* Used for pathfinding *)
module ShortestPath = Graph.Path.Dijkstra(G)(Weight)

type t = G.t [@@deriving yojson]

let make () = G.create ()

let add_ixn v ~x ~y =
  Log.debug (fun f -> f "Graph: Adding ixn at (%d,%d)" x y);
  let loc = x, y in
  G.add_vertex v loc;
  v

let remove_ixn v ~x ~y =
  Log.debug (fun f -> f "Graph: Removing ixn at (%d,%d)" x y);
  let loc = x, y in
  G.remove_vertex v loc;
  v

let add_segment v ~xyd1 ~xyd2 ~dist =
  let (x1,y1,dir1), (x2,y2,dir2) = xyd1, xyd2 in
  Log.debug (fun f -> f "Graph: Adding path from (%d,%d,%s) to (%d,%d,%s), dist %d"
              x1 y1 (Dir.show dir1) x2 y2 (Dir.show dir2) dist);
  let edge = Edge.make x1 y1 dir1 x2 y2 dir2 dist in
  G.add_edge_e v ((x1,y1), edge, (x2,y2));
  v

let get_edge v xyd =
  (* Because our edges start not just at nodes, but also at specific dirs, we have to scan
     all the options *)
  let x, y, dir = xyd in
  G.fold_succ_e (fun ((_,e,_) as edge) acc ->
    if Edge.has_xydir x y dir e then Some edge
    else acc)
    v (x,y) None

let remove_segment v ~xyd =
  let x,y,dir = xyd in
  Log.debug (fun f -> f "Graph: Remove path from (%d,%d,%s)" x y (Dir.show dir));
  (* Find the edge we want *)
  Option.iter (fun edge -> G.remove_edge_e v edge) @@ get_edge v xyd;
  v

let fold_succ_ixns f v ~ixn ~init = G.fold_succ f v ixn init
let iter_succ_ixns f v ~ixn = G.iter_succ f v ixn

(* Iterate over successors of ixn, with ixns and matching dirs *)
let iter_succ_ixn_dirs f v ~ixn =
  G.iter_succ_e (fun (ixn1, e, ixn2) ->
    let other_ixn = if Node.equal ixn ixn1 then ixn2 else ixn1 in
    let other_dir = Edge.dir_of_xy other_ixn e |> Option.get_exn_or "Missing dir" in
    f other_ixn other_dir)
  v
  ixn

(* Follow an ixn in a given dir to get the next ixn *)
let find_ixn_from_ixn_dir v ~ixn ~dir =
  let x, y = ixn in
  try
    G.fold_succ_e (fun (_, e, ixn2) acc ->
      if Edge.has_xydir x y dir e then Some ixn2 else acc)
    v ixn None
  with Invalid_argument _ -> None

  (* Find the shortest path branch from an ixn given a from_dir entering the ixn,
     which is excluded
     TODO: improve. e.g. iterate over direct branches and do multiple shortest path
     *)
let shortest_path_branch ~ixn ~cur_dir ~dest v =
  (* Block all dirs that aren't within 90 degrees of initial dir *)
  G.iter_succ_e
    (fun (_,e,_) ->
      match Edge.dir_of_xy ixn e with
      | Some dir2 when not (Dir.within_90 cur_dir dir2) ->
          Edge.set_block true e
      | _ -> ())
    v
    ixn;
  let path, _ = ShortestPath.shortest_path v ixn dest in
  (* Clear block *)
  G.iter_succ_e (fun (_,e,_) -> Edge.set_block false e) v ixn;
  match path with
  | (_,edge,_)::_ -> Edge.dir_of_xy ixn edge
  | _ -> None

let shortest_path ~src ~dest v =
  let path, _ = ShortestPath.shortest_path v src dest in
  match path with
  | (_,edge,_)::_ -> Edge.dir_of_xy src edge
  | _ -> None

module LocdSet = Utils.LocdSet
module LocSet = Utils.LocSet

  (* Get stations directly connected to a particular ixn or station
     using the track graph.
     exclude_dir: exclude searching in a given direction
     exclude_ixns: exclude these ixns from the search
   *)
let _connected_stations_dirs ~start_ixns ~seen_ixns graph trackmap =
  let stations = LocuSet.create 10 in
  let rec loop ixns =
    let next_ixns = LocSet.create 10 in
    (* Iterate over ixns we built up *)
    LocSet.iter (fun ixn ->
      if LocSet.mem seen_ixns ixn then ()
      else ( 
        (* loop over attached ixn/dirs *)
        iter_succ_ixn_dirs (fun ixn dir ->
          if Trackmap.has_station ixn trackmap then 
            (* Add to results *)
            LocuSet.insert stations (ixn, Dir.to_upper dir)
          else 
            (* To be handled in next iteration *)
            LocSet.insert next_ixns ixn)
        ~ixn
        graph
      );
      (* Mark that we saw this ixn *)
      LocSet.insert seen_ixns ixn)
      ixns;
    (* Check if done: no more ixns to examine *)
    if LocSet.cardinal next_ixns = 0 then (
     (* Remove starting ixns which might have snuck in *)
      LocSet.iter (fun ixn ->
        LocuSet.remove stations (ixn, `Upper);
        LocuSet.remove stations (ixn, `Lower);
      ) start_ixns;
      stations
    ) else
      loop next_ixns
  in
  loop start_ixns

let connected_stations_dirs_exclude_dir ~exclude_dir graph trackmap ixn =
  let start_ixns = LocSet.create 5 in
  (* Prevent loops *)
  let seen_ixns = LocSet.create 5 in
  find_ixn_from_ixn_dir graph ~ixn ~dir:exclude_dir
  |> Option.iter (LocSet.insert seen_ixns);
  LocSet.insert start_ixns ixn;
  _connected_stations_dirs ~start_ixns ~seen_ixns graph trackmap

let connected_stations_dirs ?(exclude_ixns=[]) graph trackmap ixns =
  let start_ixns = LocSet.create 5 in
  (* Prevent loops *)
  let seen_ixns = LocSet.create 5 in
  List.iter (LocSet.insert seen_ixns) exclude_ixns;
  List.iter (LocSet.insert start_ixns) ixns;
  _connected_stations_dirs ~start_ixns ~seen_ixns graph trackmap

module Track = struct
  (* open TS *)
  (* Routines to handle building/tearing down of track graph *)
  open Scan

  let handle_build_station graph ~x ~y scan1 scan2 =
    (* We just don't add stations until they've been hooked up *)
    let add_to_edge ixn1 _ ixn3 ixn4 =
      graph
      |> remove_segment ~xyd:(ixn1.x,ixn1.y,ixn1.dir)
      |> add_segment ~xyd1:(ixn3.x,ixn3.y,ixn3.dir) ~xyd2:(x,y,ixn3.search_dir) ~dist:ixn3.dist
      |> add_segment ~xyd1:(ixn4.x,ixn4.y,ixn4.dir) ~xyd2:(x,y,ixn4.search_dir) ~dist:ixn4.dist
    in
    match scan1, scan2 with
      (* Unfinished edge. Connect a station here.
          x---       ->    x---s *)
    | Track [ixn1], Station [ixn2] when equal_ixn ixn1 ixn2 ->
        add_segment ~xyd1:(ixn2.x,ixn2.y,ixn2.dir) ~xyd2:(x,y,ixn2.search_dir) ~dist:ixn2.dist graph

    (* Edge. Add a station.
      x-------x  ->    x---s---x
      Remove the edge and rebuild it to the new station.
    *)
    | Track [ixn1; ixn2], Station [ixn3; ixn4]
        when equal_ixn ixn1 ixn3 && equal_ixn ixn2 ixn4 ->
        add_to_edge ixn1 ixn2 ixn3 ixn4
    | Track [ixn1; ixn2], Station [ixn4; ixn3]
        when equal_ixn ixn1 ixn3 && equal_ixn ixn2 ixn4 ->
        add_to_edge ixn1 ixn2 ixn3 ixn4

      (* Track loop that becomes a station
          --      -S
         | |  -> | |
         --      __
       *)
    | Track [], Station [ixn1; ixn2] when equal_ixn ixn1 ixn2 ->
      graph
      |> add_segment ~xyd1:(ixn1.x,ixn1.y,ixn1.dir) ~xyd2:(x,y,ixn1.search_dir) ~dist:ixn1.dist
      |> add_segment ~xyd1:(ixn2.x,ixn2.y,ixn2.dir) ~xyd2:(x,y,ixn2.search_dir) ~dist:ixn2.dist

    | _, _ -> graph

  (* Handle simple case of track graph-wise *)
  let handle_build_track_simple graph scan1 scan2 =
    match scan1, scan2 with
      | Track [ixn1], Track [ixn2; ixn3]
          when equal_ixn ixn1 ixn2 || equal_ixn ixn1 ixn3 ->
          (* Only case: unfinished edge. Connect an intersection.
              x-- -x      ->    x----x *)
          add_segment ~xyd1:(ixn2.x,ixn2.y,ixn2.dir) ~xyd2:(ixn3.x,ixn3.y,ixn3.dir) ~dist:(ixn2.dist+ixn3.dist) graph
      | _ -> graph

  (* Handle graph management for building track.
      Complicated because we can have ixns everywhere.  *)
  let handle_build_track graph ~x ~y scan1 scan2 =
    match scan1, scan2 with
        (* Unfinished edge. Connect an intersection.
          x---       ->    x---x *)
      | Track _, Track _ ->
          handle_build_track_simple graph scan1 scan2

        (* Unfinished edge. Create an intersection.
          x---       ->    x--+ *)
      | Track [ixn1], Ixn [ixn2] when equal_ixn ixn1 ixn2 ->
          add_segment ~xyd1:(ixn2.x,ixn2.y,ixn2.dir) ~xyd2:(x,y,ixn2.search_dir) ~dist:ixn2.dist graph

        (* Regular edge. We add an intersection in the middle.
          x-----x    ->    x--+--x *)
      | Track [ixn1; ixn2], Ixn [ixn3; ixn4]
        when equal_ixn ixn1 ixn3 && equal_ixn ixn2 ixn4 ||
             equal_ixn ixn1 ixn4 && equal_ixn ixn2 ixn3 ->
          graph
          |> remove_segment ~xyd:(ixn1.x,ixn1.y,ixn1.dir)
          |> add_segment ~xyd1:(ixn3.x, ixn3.y, ixn3.dir) ~xyd2:(x, y, ixn3.search_dir) ~dist:ixn3.dist
          |> add_segment ~xyd1:(ixn4.x, ixn4.y, ixn4.dir) ~xyd2:(x, y, ixn4.search_dir) ~dist:ixn4.dist

        (* Add ixn to loop.
           ---    ->    -x-
          | |          | |
         ---          ---
        *)
      | Track [], Ixn [ixn1; ixn2] when equal_ixn ixn1 ixn2 ->
          graph
          |> add_segment ~xyd1:(ixn1.x, ixn1.y, ixn1.dir) ~xyd2:(x, y, ixn1.search_dir) ~dist:ixn1.dist
          |> add_segment ~xyd1:(ixn2.x, ixn2.y, ixn2.dir) ~xyd2:(x, y, ixn2.search_dir) ~dist:ixn2.dist

        (* Regular edge. We add an intersection in the middle that connects to another
          intersection:
            x                x
            |                |
          x-----x    ->    x--+--x *)
      | Track [ixn1; ixn2], Ixn [ixn3; ixn4; ixn5]
        when (equal_ixn ixn1 ixn3 && (equal_ixn ixn2 ixn4 || equal_ixn ixn2 ixn5))
          || (equal_ixn ixn1 ixn4 && (equal_ixn ixn2 ixn3 || equal_ixn ixn2 ixn5))
          || (equal_ixn ixn1 ixn5 && (equal_ixn ixn2 ixn3 || equal_ixn ixn2 ixn4)) ->
          graph
          |> remove_segment ~xyd:(ixn1.x,ixn1.y,ixn1.dir)
          |> add_segment ~xyd1:(ixn3.x,ixn3.y,ixn3.dir) ~xyd2:(x,y,ixn3.search_dir) ~dist:ixn3.dist
          |> add_segment ~xyd1:(ixn4.x,ixn4.y,ixn4.dir) ~xyd2:(x,y,ixn4.search_dir) ~dist:ixn4.dist
          |> add_segment ~xyd1:(ixn5.x,ixn5.y,ixn5.dir) ~xyd2:(x,y,ixn5.search_dir) ~dist:ixn5.dist

        (* All other cases require no graph changes *)
      | _ -> graph

  let handle_remove_track graph ~x ~y scan1 scan2 =
    match scan1, scan2 with
        (* Was edge. Now disconnected
          x---x       ->    x- -x *)
      | Track [ixn1; ixn2], Track [ixn3] 
          when equal_ixn ixn2 ixn3 || equal_ixn ixn1 ixn3 ->
            remove_segment ~xyd:(ixn1.x,ixn1.y,ixn1.dir) graph

      | Track [ixn1; _], NoResult ->
            remove_segment ~xyd:(ixn1.x,ixn1.y,ixn1.dir) graph

        (* Was station. Now station gone.
          x---S       ->    x---
          x---S---x   ->    x--- ---x *)
      | Station _, (Track [_] | NoResult) ->
            remove_ixn ~x ~y graph

        (* Was ixn. Now deleted.
          x---+       ->    x--- *)
      | Ixn [_], (Track [_] | NoResult) ->
            remove_ixn ~x ~y graph

        (* Was 2 ixn. Now edge
          x---+---x   ->    x-------x *)

        (* Was 3 ixn. Now edge + disconnected
              x                 x
              |                 |
          x---+---x   ->    x-------x

          x---S---x   ->    x-------x *)
      | Ixn [_; _], Track [ixn3; ixn4]
      | Ixn [_; _; _], Track [ixn3; ixn4]
      | Station [_; _], Track[ixn3; ixn4] ->
          graph
          |> remove_ixn ~x ~y
          |> add_segment ~xyd1:(ixn3.x,ixn3.y,ixn3.dir) ~xyd2:(ixn4.x,ixn4.y,ixn4.dir) ~dist:(ixn3.dist + ixn4.dist)

      | _ -> graph
        (* All other cases require no graph changes *)

end

