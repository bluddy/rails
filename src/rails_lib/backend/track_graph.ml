open Containers
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let src = Logs.Src.create "track_graph" ~doc:"The track graph"
module Log = (val Logs.src_log src: Logs.LOG)

(* Graph for intersections and stations *)

(* NOTE: must be per player! *)

module Edge = struct
  type xy_dir = (int * int * Dir.t)
  [@@deriving yojson]

  type t = {
    id: int;
    nodes: xy_dir * xy_dir;
    dist: int;
    mutable block: bool;
  } [@@deriving yojson]

  let default = {id=0; nodes=((0,0,Dir.Up),(0,0,Dir.Up)); dist=0; block=false}
  let equal x y = x.id = y.id
  let compare x y = x.id - y.id
  let make id x1 y1 dir1 x2 y2 dir2 dist =
    let nodes = ((x1,y1,dir1),(x2,y2,dir2)) in
    {id; nodes; dist; block=false}

  let eq_xydir x1 y1 dir1 (x2,y2,dir2) =
    x1 = x2 && y1 = y2 && Dir.equal dir1 dir2

    (* Either node can match *)
  let has_xydir x y dir v =
    let d1, d2 = v.nodes in
    eq_xydir x y dir d1 || eq_xydir x y dir d2

    (* Return matching dir for ixn x, y *)
  let dir_of_xy (x,y) v = match v.nodes with
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

  let weight (_,e,_) =
    if e.Edge.block then 100000
    else e.dist
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
  Log.debug (fun f -> f "Graph: Adding ixn at (%d,%d)" x y);
  G.add_vertex v.graph (x, y);
  v

let remove_ixn v ~x ~y =
  Log.debug (fun f -> f "Graph: Removing ixn at (%d,%d)" x y);
  G.remove_vertex v.graph (x, y);
  v

let add_segment v ~xyd1 ~xyd2 ~dist =
  let (x1,y1,dir1), (x2,y2,dir2) = xyd1, xyd2 in
  Log.debug (fun f -> f "Graph: Adding path from (%d,%d,%s) to (%d,%d,%s), dist %d"
              x1 y1 (Dir.show dir1) x2 y2 (Dir.show dir2) dist);
  let id = v.last_id in
  v.last_id <- succ v.last_id;
  let edge = Edge.make id x1 y1 dir1 x2 y2 dir2 dist in
  G.add_edge_e v.graph ((x1,y1),edge,(x2,y2));
  v

let remove_segment v ~xyd =
  let x, y, dir = xyd in
  Log.debug (fun f -> f "Graph: Remove path from (%d,%d,%s)" x y (Dir.show dir));
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

(* Iterate over successors of ixn, with ixns and matching dirs *)
let iter_succ_ixn_dirs f v ~ixn =
  G.iter_succ_e (fun (ixn1, e, ixn2) ->
    let other_ixn = if Node.equal ixn ixn1 then ixn2 else ixn1 in
    let other_dir = Edge.dir_of_xy other_ixn e |> Option.get_exn_or "Missing dir" in
    f other_ixn other_dir)
  v.graph
  ixn

(* Follow an ixn in a given dir to get the next ixn *)
let find_ixn_from_ixn_dir v ~ixn ~dir =
  let x, y = ixn in
  try
    G.fold_succ_e (fun (_,e,ixn2) acc ->
      if Edge.has_xydir x y dir e then Some ixn2 else acc)
    v.graph ixn None
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
          e.block <- true
      | _ -> ())
    v.graph
    ixn;
  let path, _ = ShortestPath.shortest_path v.graph ixn dest in
  (* Clear block *)
  G.iter_succ_e (fun (_,e,_) -> e.block <- false) v.graph ixn;
  match path with
  | (_,edge,_)::_ -> Edge.dir_of_xy ixn edge
  | _ -> None

let shortest_path ~src ~dest v =
  let path, _ = ShortestPath.shortest_path v.graph src dest in
  match path with
  | (_,edge,_)::_ -> Edge.dir_of_xy src edge
  | _ -> None

  (* Get stations directly connected to a particular ixn or station
     using the track graph.
     exclude_dir: exclude searching in a given direction
     exclude_ixns: exclude these ixns from the search
   *)
let connected_stations_dirs ?exclude_dir ?exclude_ixns graph station_map ixn =
  let stations = Hashtbl.create 10 in
  let start_ixns = Hashtbl.create 5 in
  (* Prevent loops *)
  let seen_ixns = Hashtbl.create 5 in
  begin match exclude_dir, exclude_ixns with
  | Some exclude_dir, _ ->
      begin match find_ixn_from_ixn_dir graph ~ixn ~dir:exclude_dir with
      | Some ixn2 ->
          Hashtbl.replace seen_ixns ixn2 ()
      | _ -> ()
      end;
  | _, Some exclude_ixns ->
      (* Exclude some ixns and start from given ixn *)
      List.iter (fun ixn -> Hashtbl.replace seen_ixns ixn ()) exclude_ixns;
  | _ -> ()
  end;
  Hashtbl.replace start_ixns ixn ();
  let rec loop ixns =
    let ixns2 = Hashtbl.create 10 in
    Hashtbl.iter (fun ixn _ ->
      if Hashtbl.mem seen_ixns ixn then ()
      else begin 
        iter_succ_ixn_dirs (fun ixn dir ->
          if Station_map.mem ixn station_map then 
            Hashtbl.replace stations ixn dir
          else 
            Hashtbl.replace ixns2 ixn ())
        ~ixn
        graph
      end;
      Hashtbl.replace seen_ixns ixn ())
      ixns;
    (* Check if done: no more ixns to examine *)
    if Hashtbl.length ixns2 = 0 then begin
      Hashtbl.remove stations ixn;
      Hashtbl.to_iter stations
    end else loop ixns2
  in
  loop start_ixns

module Track = struct
  (* open TS *)
  (* Routines to handle building/tearing down of track graph *)
  open Trackmap.Search

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
    | _, _ -> graph

  (* Handle simple building of track graph-wise *)
  let handle_build_track graph scan1 scan2 =
    match scan1, scan2 with
      | Track [ixn1], Track [ixn2; ixn3]
          when equal_ixn ixn1 ixn2 || equal_ixn ixn1 ixn3 ->
          (* Only case: unfinished edge. Connect an intersection.
              x---       ->    x---x *)
          add_segment ~xyd1:(ixn2.x,ixn2.y,ixn2.dir) ~xyd2:(ixn3.x,ixn3.y,ixn3.dir)
                        ~dist:(ixn2.dist+ixn3.dist) graph
      | _ -> graph

  (* Handle graph management for building track.
      Complicated because we can have ixns everywhere. 
      TODO: check this for Station *)
    
  let handle_build_track_complex graph ~x ~y scan1 scan2 =
    match scan1, scan2 with
        (* Unfinished edge. Connect an intersection.
          x---       ->    x---x *)
      | Track [ixn1], Track [ixn2; ixn3]
          when equal_ixn ixn1 ixn2 || equal_ixn ixn1 ixn3 ->
            add_segment ~xyd1:(ixn2.x,ixn2.y,ixn2.dir)
                          ~xyd2:(ixn3.x,ixn3.y,ixn3.dir)
                          ~dist:(ixn2.dist+ixn3.dist)
                          graph
        (* Unfinished edge. Create an intersection.
          x---       ->    x--+ *)
      | Track [ixn1], Ixn [ixn2] when equal_ixn ixn1 ixn2 ->
          add_segment ~xyd1:(ixn2.x,ixn2.y,ixn2.dir)
                        ~xyd2:(x,y,ixn2.search_dir)
                        ~dist:ixn2.dist
                        graph
        (* Regular edge. We add an intersection in the middle.
          x-----x    ->    x--+--x *)
      | Track [ixn1; ixn2], Ixn [ixn3; ixn4]
        when equal_ixn ixn1 ixn3 && equal_ixn ixn2 ixn4 ||
             equal_ixn ixn1 ixn4 && equal_ixn ixn2 ixn3 ->
          graph
          |> remove_segment ~xyd:(ixn1.x,ixn1.y,ixn1.dir)
          |> add_segment ~xyd1:(ixn3.x, ixn3.y, ixn3.dir)
                           ~xyd2:(x, y, ixn3.search_dir)
                           ~dist:ixn3.dist
          |> add_segment ~xyd1:(ixn4.x, ixn4.y, ixn4.dir)
                           ~xyd2:(x, y, ixn4.search_dir)
                           ~dist:ixn4.dist
                          

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
          |> add_segment ~xyd1:(ixn3.x,ixn3.y,ixn3.dir)
                           ~xyd2:(x,y,ixn3.search_dir)
                           ~dist:ixn3.dist
          |> add_segment ~xyd1:(ixn4.x,ixn4.y,ixn4.dir)
                           ~xyd2:(x,y,ixn4.search_dir)
                           ~dist:ixn4.dist
          |> add_segment ~xyd1:(ixn5.x,ixn5.y,ixn5.dir)
                           ~xyd2:(x,y,ixn5.search_dir)
                           ~dist:ixn5.dist
      | _ -> graph
        (* All other cases require no graph changes *)

  let handle_remove_track graph ~x ~y scan1 scan2 =
    match scan1, scan2 with
        (* Was edge. Now disconnected
          x---x       ->    x- -x *)
      | Track [ixn1; ixn2], Track [ixn3]
          when equal_ixn ixn2 ixn3 || equal_ixn ixn1 ixn3 ->
            remove_segment ~xyd:(ixn1.x,ixn1.y,ixn1.dir) graph

        (* Was station. Now station gone.
          x---S       ->    x--- *)
      | Station [_], (Track [_] | NoResult) ->
            remove_ixn ~x ~y graph

        (* Was ixn. Now deleted.
          x---+       ->    x--- *)
      | Ixn [_], (Track [_] | NoResult) ->
            remove_ixn ~x ~y graph

        (* Was connecting station. Now disconnected
          x---S---x   ->    x--- ---x *)
      | Station [_; _], Track[_] ->
            remove_ixn ~x ~y graph

        (* Was 2 ixn. Now edge
          x---+---x   ->    x-------x *)

        (* Was 3 ixn. Now edge + disconnected
              x                 x
              |                 |
          x---+---x   ->    x-------x *)
      | Ixn [_; _], Track [ixn3; ixn4]
      | Ixn [_; _; _], Track [ixn3; ixn4] ->
          graph
          |> remove_ixn ~x ~y
          |> add_segment ~xyd1:(ixn3.x,ixn3.y,ixn3.dir)
                           ~xyd2:(ixn4.x,ixn4.y,ixn4.dir)
                           ~dist:(ixn3.dist + ixn4.dist)
      | _ -> graph
        (* All other cases require no graph changes *)

end

