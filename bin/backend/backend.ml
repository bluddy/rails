open Containers
open Utils.Infix

module TS = Trackmap.Search
module G = Track_graph
module C = Constants

(* This is the backend. All game-modifying functions go through here *)

(* The actual game (server) state
   Observers can observe data in the backend,
   but actions can only be taken via messages (Backend.Action)
 *)

let tick_ms = 15 (* ms *)
let year_ticks = 2032 (* really 170*12 = 2040 is new year *)
let month_ticks = 170
let num_players = 4


(* Cycle counts to perform some tasks *)
let cycles_periodic_maintenance = 1024
let cycles_priority_delivery = 8
let cycles_background_update = 16
let cycles_ai_update = cycles_background_update
(* In the original game, we do slices of 1/32 stations. No need *)
let cycles_station_supply_demand = cycles_background_update * 32 (* 512 *)
(* Since we don't spread out the supply addition, decay happens in the same cycle *)
let cycles_supply_decay = 512


type ui_msg =
  | TrainBuilt of int
  | DemandChanged of {x: int; y: int; good: Goods.t; add: bool}
  [@@deriving yojson]

type loc = int * int (* x, y *)
  [@@ deriving yojson]

type t = {
  mutable last_tick: int; (* last time we updated a cycle *)
  mutable cycle: int; (* counter used for all sorts of per-tick updates *)
  mutable time: int;  (* In-game time, resets at end of year *)
  mutable year: int;
  fiscal_period: [`First | `Second];
  climate: Climate.t;
  players: Player.t array;
  region: Region.t;
  map : Tilemap.t;
  mutable track: Trackmap.t;
  mutable graph: Track_graph.t;
  trains: Trainmap.t;
  cities: Cities.t;
  mutable stations: Station.t Loc_map.t;
  segments: Segment.Map.t; (* map segments btw stations *)
  priority: (loc * loc * Goods.t) option;  (* priority shipment *)
  stats: Stats.t;
  options: B_options.t;
  mutable ui_msgs: ui_msg list;
  random: Utils.Random.State.t;
  seed: int;
} [@@deriving yojson]

let default region resources ~random ~seed = 
  let map = List.assoc ~eq:(Stdlib.(=)) region resources.Resources.res_maps in
  let map = Tilemap.of_ndarray ~region ~seed map in
  let width, height = Tilemap.get_width map, Tilemap.get_height map in
  let cities =
    let h = Hashtbl.create 100 in
    List.assoc ~eq:(Stdlib.(=)) region resources.res_cities
    |> List.iter (fun (name,x,y) -> Hashtbl.replace h (y * width + x) name);
    Cities.make h width height
  in
  let track = Trackmap.empty width height in
  let options = B_options.default in
  let stations = Loc_map.create width in
  let players = Array.make num_players Player.default in
  let year = match region with
    | EastUS -> 1830
    | WestUS -> 1866
    | Britain -> 1828
    | Europe -> 1900
  in
  let trains = Trainmap.empty () in
  let graph = Track_graph.make () in
  {
    time=0;
    cycle=0;
    last_tick=0;
    year;
    fiscal_period=`First;
    climate=Moderation;
    players;
    map;
    region;
    cities;
    trains;
    track;
    segments=Segment.Map.make ();
    graph;
    stations;
    priority=None;
    options;
    ui_msgs = [];
    random;
    seed;

    stats=Stats.default;
  }

let modify_player v ~player f =
  v.players.(player) <- f (v.players.(player))

let get_speed v = v.options.speed

let _set_speed v speed = {v with options={v.options with speed}}

let map_height v = Tilemap.get_height v.map

let map_width v = Tilemap.get_width v.map

let get_tile v x y = Tilemap.get_tile v.map x y

let get_track v x y = Trackmap.get v.track x y

let get_cities v = Cities.to_list v.cities

let get_station v x y = Loc_map.get v.stations x y

let get_region v = v.region

let get_map v = v.map

let get_tile_height v x y = Tilemap.get_tile_height v.map x y

let iter_cities f v = Cities.iter f v.cities

let find_close_city v x y ~range = Cities.find_close v.cities x y ~range

let check_build_track v ~x ~y ~dir ~player =
  (* First check the tilemap, then the trackmap *)
  match Tilemap.check_build_track v.map ~x ~y ~dir with
  | `Bridge when Trackmap.check_build_stretch v.track ~x ~y ~dir ~player ~length:2 -> `Bridge
  | `Tunnel(length, _) as tun when Trackmap.check_build_stretch v.track ~x ~y ~dir ~player ~length -> tun
  | (`Tunnel(_,g) | `HighGrade g) when Trackmap.check_build_track v.track ~x ~y ~dir ~player -> `HighGrade g
  | (`Ok | `Ferry) as ret when Trackmap.check_build_track v.track ~x ~y ~dir ~player -> ret
  | _ -> `Illegal

let get_money v ~player = Player.get_money v.players.(player)

module Graph = struct
  open TS
  (* Routines to handle building/tearing down of track graph *)

  let handle_build_station v ~x ~y scan1 scan2 =
    (* We just don't add stations until they've been hooked up *)
    let add_to_edge ixn1 _ ixn3 ixn4 =
      v.graph
      |> G.remove_segment ~x:ixn1.x ~y:ixn1.y ~dir:ixn1.dir
      |> G.add_segment ~x1:ixn3.x ~y1:ixn3.y ~dir1:ixn3.dir
                        ~x2:x ~y2:y ~dir2:ixn3.search_dir
                        ~dist:ixn3.dist
      |> G.add_segment ~x1:ixn4.x ~y1:ixn4.y ~dir1:ixn4.dir
                        ~x2:x ~y2:y ~dir2:ixn4.search_dir
                        ~dist:ixn4.dist
    in
    match scan1, scan2 with
      (* Unfinished edge. Connect a station here.
          x---       ->    x---s *)
    | Track [ixn1], Station [ixn2] when TS.(eq ixn1 ixn2) ->
        G.add_segment ~x1:ixn2.x ~y1:ixn2.y ~dir1:ixn2.dir
                      ~x2:x ~y2:y ~dir2:ixn2.search_dir ~dist:ixn2.dist
                      v.graph

    (* Edge. Add a station.
      x-------x  ->    x---s---x
      Remove the edge and rebuild it to the new station.
    *)
    | Track [ixn1; ixn2], Station [ixn3; ixn4]
        when TS.(eq ixn1 ixn3 && eq ixn2 ixn4) ->
        add_to_edge ixn1 ixn2 ixn3 ixn4
    | Track [ixn1; ixn2], Station [ixn4; ixn3]
        when TS.(eq ixn1 ixn3 && eq ixn2 ixn4) ->
        add_to_edge ixn1 ixn2 ixn3 ixn4
    | _, _ -> v.graph

  (* Handle simple building of track graph-wise *)
  let handle_build_track v scan1 scan2 =
    match scan1, scan2 with
      | Track [ixn1], Track [ixn2; ixn3]
          when TS.(eq ixn1 ixn2 || eq ixn1 ixn3) ->
          (* Only case: unfinished edge. Connect an intersection.
              x---       ->    x---x *)
          G.add_segment ~x1:ixn2.x ~y1:ixn2.y ~dir1:ixn2.dir
                        ~x2:ixn3.x ~y2:ixn3.y ~dir2:ixn3.dir
                        ~dist:(ixn2.dist+ixn3.dist)
                        v.graph
      | _ -> v.graph

  (* Handle graph management for building track.
      Complicated because we can have ixns everywhere. 
      TODO: check this for Station *)
    
  let handle_build_track_complex v ~x ~y scan1 scan2 =
    match scan1, scan2 with
        (* Unfinished edge. Connect an intersection.
          x---       ->    x---x *)
      | Track [ixn1], Track [ixn2; ixn3]
          when TS.(eq ixn1 ixn2 || TS.eq ixn1 ixn3) ->
            G.add_segment ~x1:ixn2.x ~y1:ixn2.y ~dir1:ixn2.dir
                          ~x2:ixn3.x ~y2:ixn3.y ~dir2:ixn3.dir
                          ~dist:(ixn2.dist+ixn3.dist)
                          v.graph
        (* Unfinished edge. Create an intersection.
          x---       ->    x--+ *)
      | Track [ixn1], Ixn [ixn2] when TS.eq ixn1 ixn2 ->
          G.add_segment ~x1:ixn2.x ~y1:ixn2.y ~dir1:ixn2.dir
                        ~x2:x ~y2:y ~dir2:ixn2.search_dir
                        ~dist:ixn2.dist
                        v.graph
        (* Regular edge. We add an intersection in the middle.
          x-----x    ->    x--+--x *)
      | Track [ixn1; ixn2], Ixn [ixn3; ixn4]
        when TS.(eq ixn1 ixn3 && eq ixn2 ixn4 || eq ixn1 ixn4 && eq ixn2 ixn3) ->
          v.graph
          |> G.remove_segment ~x:ixn1.x ~y:ixn1.y ~dir:ixn1.dir
          |> G.add_segment ~x1:ixn3.x ~y1:ixn3.y ~dir1:ixn3.dir
                          ~x2:x ~y2:y ~dir2:ixn3.search_dir
                          ~dist:ixn3.dist
          |> G.add_segment ~x1:ixn4.x ~y1:ixn4.y ~dir1:ixn4.dir
                          ~x2:x ~y2:y ~dir2:ixn4.search_dir
                          ~dist:ixn4.dist
                          

        (* Regular edge. We add an intersection in the middle that connects to another
          intersection:
            x                x
            |                |
          x-----x    ->    x--+--x *)
      | Track [ixn1; ixn2], Ixn [ixn3; ixn4; ixn5]
        when TS.((eq ixn1 ixn3 && (eq ixn2 ixn4 || eq ixn2 ixn5))
          || (eq ixn1 ixn4 && (eq ixn2 ixn3 || eq ixn2 ixn5))
          || (eq ixn1 ixn5 && (eq ixn2 ixn3 || eq ixn2 ixn4))) ->
          v.graph
          |> G.remove_segment ~x:ixn1.x ~y:ixn1.y ~dir:ixn1.dir
          |> G.add_segment ~x1:ixn3.x ~y1:ixn3.y ~dir1:ixn3.dir
                          ~x2:x ~y2:y ~dir2:ixn3.search_dir
                          ~dist:ixn3.dist
          |> G.add_segment ~x1:ixn4.x ~y1:ixn4.y ~dir1:ixn4.dir
                          ~x2:x ~y2:y ~dir2:ixn4.search_dir
                          ~dist:ixn4.dist
          |> G.add_segment ~x1:ixn5.x ~y1:ixn5.y ~dir1:ixn5.dir
                          ~x2:x ~y2:y ~dir2:ixn5.search_dir
                          ~dist:ixn5.dist
      | _ -> v.graph
        (* All other cases require no graph changes *)

  let handle_remove_track v ~x ~y scan1 scan2 =
    match scan1, scan2 with
        (* Was edge. Now disconnected
          x---x       ->    x- -x *)
      | Track [ixn1; ixn2], Track [ixn3]
          when TS.(eq ixn2 ixn3 || TS.eq ixn1 ixn3) ->
            G.remove_segment ~x:ixn1.x ~y:ixn1.y ~dir:ixn1.dir v.graph

        (* Was station. Now station gone.
          x---S       ->    x--- *)
      | Station [_], (Track [_] | NoResult) ->
            G.remove_ixn ~x ~y v.graph

        (* Was ixn. Now deleted.
          x---+       ->    x--- *)
      | Ixn [_], (Track [_] | NoResult) ->
            G.remove_ixn ~x ~y v.graph

        (* Was connecting station. Now disconnected
          x---S---x   ->    x--- ---x *)
      | Station [_; _], Track[_] ->
            G.remove_ixn ~x ~y v.graph

        (* Was 2 ixn. Now edge
          x---+---x   ->    x-------x *)

        (* Was 3 ixn. Now edge + disconnected
              x                 x
              |                 |
          x---+---x   ->    x-------x *)
      | Ixn [_; _], Track [ixn3; ixn4]
      | Ixn [_; _; _], Track [ixn3; ixn4] ->
          v.graph
          |> G.remove_ixn ~x ~y
          |> G.add_segment ~x1:ixn3.x ~y1:ixn3.y ~dir1:ixn3.dir
                          ~x2:ixn4.x ~y2:ixn4.y ~dir2:ixn4.dir
                          ~dist:(ixn3.dist + ixn4.dist)
      | _ -> v.graph
        (* All other cases require no graph changes *)

    (* Get stations directly connected to a particular ixn or station
       using the track graph.
       search_dir: start searching in a given direction
       exclude_ixns: exclude these ixns from the search
     *)
  let connected_stations_dirs ?search_dir ?exclude_ixns v ixn =
    let stations = Hashtbl.create 10 in
    let start_ixns = Hashtbl.create 5 in
    (* Prevent loops *)
    let seen_ixns = Hashtbl.create 5 in
    begin match search_dir, exclude_ixns with
    | Some dir, _ ->
        begin match Track_graph.find_ixn_from_ixn_dir v.graph ~ixn ~dir with
        | Some ixn2 ->
            Hashtbl.replace start_ixns ixn2 ()
        | None -> ()
        end;
        (* Avoid going back to original ixn *)
        Hashtbl.replace seen_ixns ixn ();
    | _, Some exclude_ixns ->
        (* Exclude some ixns and start from given ixn *)
        List.iter (fun ixn -> Hashtbl.replace seen_ixns ixn ()) exclude_ixns;
        Hashtbl.replace start_ixns ixn ()
    | None, None ->
        Hashtbl.replace start_ixns ixn ()
    end;
    let rec loop ixns =
      let ixns2 = Hashtbl.create 10 in
      Hashtbl.iter (fun ixn _ ->
        if Hashtbl.mem seen_ixns ixn then ()
        else begin 
          Track_graph.iter_succ_ixn_dirs (fun ((x,y) as ixn) dir ->
            if Loc_map.mem v.stations x y then 
              Hashtbl.replace stations ixn dir
            else 
              Hashtbl.replace ixns2 ixn ())
          ~ixn
          v.graph
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
end

module StationSegments = struct
  (* Module to handle the connections between stations (segments). We use these
     to make sure the 'semaphore' for the track has the right number of trains on it.
   *)

  (* When we build a station, we create new station segments on both ends of the station *)
  let build_station_get_segments v track x y =
    let tile = Trackmap.get_exn track x y in
    Dir.Set.fold (fun acc dir ->
      match Graph.connected_stations_dirs ~search_dir:dir v (x,y) |> Iter.head with
      | Some ((x,y), station_dir) ->
          let station = Loc_map.get_exn v.stations x y in
          let seg = Station.get_segment station station_dir in
          (dir, seg)::acc
      | None ->
          let seg = Segment.Map.get_id v.segments in
          (dir, seg)::acc)
     []
     tile.dirs

  (* We only care about connecting to a new piece of track that could lead
  to a station. ixns and stations are the same for this *)
  let build_track_join_segments (v:t) track x y (scan1:TS.scan) (scan2:TS.scan) =
    let join_ixns = match scan1, scan2 with
      (* Add an attached ixn: make the two have the same segment *)
      | Track [ixn1], Track [ixn2; ixn3] -> Some (ixn2, ixn3)
      (* Add an ixn to a 2-ixn. Make them all have same segment *)
      | Track (ixn1::_ as l1), Ixn l2 ->
          begin match Utils.find_mismatch ~eq:TS.eq ~left:l2 ~right:l1 with
          | Some ixn2 -> Some (ixn1, ixn2)
          | None -> None
          end
      | _ -> None
    in
    match join_ixns with
    | None -> ()
    | Some (ixn1, ixn2) ->
        let ixn1 = (ixn1.x, ixn1.y) in
        let ixn2 = (ixn2.x, ixn2.y) in
        let (x1, y1), dir1 =
          Graph.connected_stations_dirs v ixn1 ~exclude_ixns:[ixn2]
          |> Iter.head_exn
        in
        let station1 = Loc_map.get_exn v.stations x1 y1 in
        let seg1 = Station.get_segment station1 dir1 in
        let stations =
          Graph.connected_stations_dirs v ixn2 ~exclude_ixns:[ixn1]
          |> Iter.to_list
        in
        let (x2, y2), dir2 = List.hd stations in
        let station2 = Loc_map.get_exn v.stations x2 y2 in
        let seg2 = Station.get_segment station2 dir2 in
        (* Assign seg1 to all connected stations that had seg2 *)
        List.iter (fun ((x, y) as ixn, _) ->
            Loc_map.update v.stations x y @@
              (Option.map (fun station -> Station.modify_segment station seg2 seg1)))
          stations;
        (* Update segment map *)
        Segment.Map.merge v.segments seg1 seg2;
        ()

    (* Removing a piece of track can split a segment. Unfortunately we can't
       keep track of the segment's semaphore value unless we scan the whole segment for
       trains.
       We're too lazy to do that so we'll just set all segment values to 0.
       NOTE: This can cause train crashes.
     *)
  let remove_track_split_segment v track x y (before:TS.scan) (after:TS.scan) =
    let separate_pair = match before, after with
      (* Disconnecting a track leading to 2 ixns *)
      | Track [ixn1; ixn2], Track [_] -> Some(ixn1, ixn2)
      (* Disconnecting an ixn *)
      | Ixn l1, Track ((ixn2::_) as l2) ->
          begin match Utils.find_mismatch ~eq:TS.eq ~left:l1 ~right:l2 with
          | Some ixn1 -> Some (ixn1, ixn2)
          | None -> assert false
          end
      (* Removing a station *)
      | Station [ixn1; ixn2], _ -> Some (ixn1, ixn2)
      | _ -> None
    in
    match separate_pair with
    | None -> ()
    | Some (ixn1, ixn2) ->
        let ixn1 = (ixn1.x, ixn1.y) in
        let ixn2 = (ixn2.x, ixn2.y) in
        let (x1, y1), dir1 =
          Graph.connected_stations_dirs v ixn1 |> Iter.head_exn
        in
        let station1 = Loc_map.get_exn v.stations x1 y1 in
        let seg1 = Station.get_segment station1 dir1 in
        (* Set value of segment to 0 *)
        Segment.Map.reset v.segments seg1;
        (* Create a new segment for the split segment *)
        let seg2 = Segment.Map.get_id v.segments in

        let stations = Graph.connected_stations_dirs v ixn2 in
        (* Assign seg2 to these stations *)
        Iter.iter (fun ((x, y), _) ->
            Loc_map.update v.stations x y @@
              (Option.map (fun station -> Station.modify_segment station seg1 seg2)))
          stations;
        ()

end

let _build_tunnel v ~x ~y ~dir ~player ~length =
  let scan1 = TS.scan v.track ~x ~y ~player in
  let track = Trackmap.build_tunnel v.track ~x ~y ~dir ~player ~length in
  let scan2 = TS.scan track ~x ~y ~player in
  let graph = Graph.handle_build_track v scan1 scan2 in
  modify_player v ~player (Player.add_track ~length);
  if v.track =!= track then v.track <- track;
  if v.graph =!= graph then v.graph <- graph;
  v

let check_build_station v ~x ~y ~player station_type =
  match Trackmap.check_build_station v.track ~x ~y ~player station_type with
  | `Ok -> Tilemap.check_build_station v.map ~x ~y
  | x -> x

let _build_station v ~x ~y station_type ~player =
  let before = TS.scan v.track ~x ~y ~player in
  let track, build_new_track = Trackmap.build_station v.track ~x ~y station_type in
  let after = TS.scan track ~x ~y ~player in
  let graph = Graph.handle_build_station v ~x ~y before after in
  let segments = StationSegments.build_station_get_segments v track x y in
  let city = find_close_city ~range:100 v x y |> Option.get_exn_or "error" in
  let check_for_first_city () =
    (* first one has engine shop *)
    match
      Loc_map.filter v.stations (Station.has_upgrade ~upgrade:Station.EngineShop)
      |> Iter.head
    with Some _ -> false | None -> true
  in
  let first = check_for_first_city () in
  let station =
    Station.make ~x ~y
      ~year:v.year
      ~name:city
      ~kind:station_type
      ~player
      ~first
      ~segments
  in
  let stations = Loc_map.add v.stations x y station in
  if build_new_track then (
    modify_player v ~player @@ Player.add_track ~length:1
  );

  (* Initialize supply and demand *)
  let simple_economy =
    not @@ B_options.RealityLevels.mem v.options.reality_levels `ComplexEconomy 
  in
  let climate = v.climate in
  ignore @@ Station.update_supply_demand station v.map ~climate ~simple_economy;

  if v.track =!= track then v.track <- track;
  if v.graph =!= graph then v.graph <- graph;
  if v.stations =!= stations then v.stations <- stations;
  v

let check_build_bridge v ~x ~y ~dir ~player =
  match check_build_track v ~x ~y ~dir ~player with
  | `Bridge -> `Ok
  | _ -> `Illegal

let _build_bridge v ~x ~y ~dir ~player ~kind =
  let scan1 = TS.scan v.track ~x ~y ~player in
  let track = Trackmap.build_bridge v.track ~x ~y ~dir ~player ~kind in
  modify_player v ~player (Player.add_track ~length:2);
  let scan2 = TS.scan track ~x ~y ~player in
  let graph = Graph.handle_build_track v scan1 scan2 in
  if v.track =!= track then v.track <- track;
  if v.graph =!= graph then v.graph <- graph;
  v

let _build_track (v:t) ~x ~y ~dir ~player =
  (* Can either create a new edge or a new node (ixn) *)
  let scan1 = TS.scan v.track ~x ~y ~player in
  let track = Trackmap.build_track v.track ~x ~y ~dir ~player in
  modify_player v ~player (Player.add_track ~length:1);
  let scan2 = TS.scan track ~x ~y ~player in
  let graph = Graph.handle_build_track_complex v ~x ~y scan1 scan2 in
  if v.track =!= track then v.track <- track;
  if v.graph =!= graph then v.graph <- graph;
  v

let _build_ferry v ~x ~y ~dir ~player =
  let scan1 = TS.scan v.track ~x ~y ~player in
  let tile1 = get_tile v x y in
  let dx, dy = Dir.to_offsets dir in
  let tile2 = get_tile v (x+dx) (y+dy) in
  let kind1, kind2 = match tile1, tile2 with
    | Tile.Ocean _ , Ocean _ -> Track.Ferry, Track.Ferry
    | Ocean _, _ -> Ferry, Track
    | _, Ocean _ -> Track, Ferry
    | _ -> assert false
  in
  let track = Trackmap.build_track v.track ~x ~y ~dir ~player ~kind1 ~kind2 in
  modify_player v ~player (Player.add_track ~length:1);
  let scan2 = TS.scan track ~x ~y ~player in
  let graph = Graph.handle_build_track v scan1 scan2 in
  if v.track =!= track then v.track <- track;
  if v.graph =!= graph then v.graph <- graph;
  v

let check_remove_track v ~x ~y ~dir ~player=
  Trackmap.check_remove_track v.track ~x ~y ~dir ~player

let _remove_track v ~x ~y ~dir ~player =
  let scan1 = TS.scan v.track ~x ~y ~player in
  let track = Trackmap.remove_track v.track ~x ~y ~dir ~player in
  let scan2 = TS.scan track ~x ~y ~player in
  let graph = Graph.handle_remove_track v ~x ~y scan1 scan2 in
  modify_player v ~player (Player.add_track ~length:(-1));
  if v.track =!= track then v.track <- track;
  if v.graph =!= graph then v.graph <- graph;
  v

let _improve_station v ~x ~y ~player ~upgrade =
  let stations = 
    match get_station v x y with
    | Some station ->
        let station = Station.add_upgrade station upgrade player in
        Loc_map.add v.stations x y station
    | None -> v.stations
  in
  if v.stations =!= stations then v.stations <- stations;
  v

let _build_train v ((x, y) as station) engine cars other_station =
  let engine_t = Engine.t_of_make v.region engine in
  (* Temporary solution for getting track dir *)
  let track = Trackmap.get v.track x y |> Option.get_exn in
  let dir, _ = Dir.Set.pop track.dirs in
  let train = Train.make station engine_t cars other_station ~dir in
  let trains = Trainmap.add v.trains train in
  let msg = TrainBuilt (Trainmap.size v.trains - 1) in
  v.ui_msgs <- msg::v.ui_msgs;
  if trains === v.trains then v else {v with trains}

let _remove_stop_car v ~train ~stop ~car =
  let trains =
    Trainmap.update v.trains train
      (fun train -> Train.remove_stop_car train stop car)
  in
  if trains =!= v.trains then {v with trains} else v

let check_stop_station v ~train ~stop ~station =
  let train = Trainmap.get v.trains train in
  Train.check_stop_station train stop station

let _set_stop_station v ~train ~stop ~station =
  let trains =
    Trainmap.update v.trains train
      (fun train -> Train.set_stop_station train stop station)
  in
  if trains =!= v.trains then {v with trains} else v

let _remove_stop v ~train ~stop =
  let trains =
    Trainmap.update v.trains train
      (fun train -> Train.remove_stop train stop)
  in
  if trains =!= v.trains then {v with trains} else v

let _add_stop_car v ~train ~stop ~car =
  let trains =
    Trainmap.update v.trains train
      (fun train -> Train.add_stop_car train stop car)
  in
  if trains =!= v.trains then {v with trains} else v

let _remove_all_stop_cars v ~train ~stop =
  let trains =
    Trainmap.update v.trains train
      (fun train -> Train.remove_all_stop_cars train stop)
  in
  if trains =!= v.trains then {v with trains} else v

let get_num_trains v = Trainmap.size v.trains

let get_train v idx = Trainmap.get v.trains idx
  
let trackmap_iter v f = Trackmap.iter v.track f

let update_train_mid_tile ~idx ~cycle (v:t) (train:Train.t) =
  let tile_x, tile_y = train.x / C.tile_w, train.y / C.tile_h in
  
  (* TODO: check for colocated trains *)

  let track = Trackmap.get_exn v.track tile_x tile_y in
  (* Adjust direction *)
  let dir =
    if track.ixn then
      let dest = Train.get_dest train in
      Track_graph.shortest_path_branch v.graph
        ~ixn:(tile_x, tile_y) ~dir:train.dir ~dest 
        |> Option.get_exn_or "Cannot find route" 
    else
      Dir.find_nearest_in_set train.dir track.dirs
      |> Option.get_exn_or "Cannot find track for train"
  in
  (* Speed factor computation *)
  let height1 = Tilemap.get_tile_height v.map tile_x tile_y in
  let tile_x2, tile_y2 = Dir.adjust dir tile_x tile_y in
  let track2 = Trackmap.get_exn v.track tile_x2 tile_y2 in
  let height2 = Tilemap.get_tile_height v.map tile_x2 tile_y2 in
  let d_height = max 0 (height2 - height1) in
  let d_height = if Dir.is_diagonal dir then d_height else d_height * 3/2 in
  let height_factor = match track.kind, track2.kind with
    | Bridge _ , _ -> 0
    | _, Tunnel -> 0
    | _ -> d_height
  in
  let turn_factor = Dir.diff dir train.dir in
  let speed_factor = (height_factor * height_factor / 144) + turn_factor in
  train.dir <- dir;

  Train.History.add train.history train.x train.y train.dir speed_factor;
  let weight = Train.get_weight train in
  let max_speed_factor = Train.get_max_speed_factor train in
  Train.target_speed_from_factors train ~idx ~cycle ~weight ~max_speed_factor;
  begin match Tilemap.get_tile v.map tile_x tile_y with
  | Ocean _ | Harbor _ ->
      train.target_speed <- 1;
      train.speed <- 1;
  | _ -> ()
  end;
  let dist = if Dir.is_diagonal dir then 2 else 3 in
  Train.add_dist_traveled train dist v.fiscal_period;
  v.stats.dist_traveled <- succ v.stats.dist_traveled;
  ()

  (* Run every cycle, updating every train's position and speed *)
let update_all_trains (v:t) =
  let cycle_check, region_div = if Region.is_us v.region then 16, 1 else 8, 2 in
  let cycle_bit = 1 lsl ((v.cycle / 16) mod 12) in

  Trainmap.foldi (fun idx max_priority (train:Train.t) ->
    let priority = (Goods.freight_to_enum train.freight) * 3 -
                   (Train.train_type_to_enum train._type) + 2 in
    if priority <= max_priority && train.wait_time > 0 && train.speed > 0 then begin
      Train.update_speed train ~cycle:v.cycle ~cycle_check ~cycle_bit;

      (* Todo: fiscal period update stuff *)

      (* Adjust location based on speed / 12 *)
      let rec loop mult_12 =
        if mult_12 * 12 >= train.speed then ()
        else
          let speed =
            if Dir.is_diagonal train.dir then ((train.speed * 2) + 1) / 3
            else train.speed
          in
          let speed =
            if speed > 12 then if mult_12 = 0 then 12 else speed - 12 else speed
          in
          let speed = speed / region_div |> Utils.clip ~min:1 ~max:99 in

          let mid_tile_check () =
            train.x mod C.tile_w = C.tile_w / 2 &&
            train.y mod C.tile_h = C.tile_h / 2
          in

          if Train.update_cycle_array.(speed) land cycle_bit <> 0 then begin
            (* Check if we're in the middle of a tile.
               This is where the advanced processing happens.
             *)
            if mid_tile_check () then (
               update_train_mid_tile ~idx ~cycle:v.cycle v train;
            );

            (* Always advance train by single pixel *)
            let dx, dy = Dir.to_offsets train.dir in
            train.x <- train.x + dx;
            train.y <- train.y + dy;
          end;

          loop (mult_12 + 1)
      in
      loop 0;
      priority
    end else
      priority)
  v.trains
  ~init:0

  (** Most time-based work happens here **)
let handle_cycle v =
  v.cycle <- v.cycle + 1;

  (* TODO: ai_routines *)

  let demand_msgs =
    if v.cycle mod cycles_station_supply_demand = 0 then (
      Printf.printf "handle_cycle%!\n";
      let difficulty = v.options.difficulty in
      let climate = v.climate in
      let simple_economy =
        not @@ B_options.RealityLevels.mem v.options.reality_levels `ComplexEconomy 
      in
      Loc_map.fold 
        (fun station old_msgs ->
          Station.check_rate_war_lose_supplies station ~difficulty;
          let msgs =
            Station.update_supply_demand station v.map ~climate ~simple_economy
          in
          Station.lose_supplies station;
          let msgs =
            List.map (fun (good, add) ->
                DemandChanged {x=station.x; y=station.y; good; add})
              msgs
          in
          msgs @ old_msgs)
      v.stations
      ~init:[]
    )
    else []
  in

  (* adjust time *)
  v.time <- v.time + 1;
  let v = 
    if v.time >= year_ticks then
      {v with year=v.year + 1}
    else v
  in
  v, demand_msgs

let handle_tick v cur_time =
  (* TODO: handle pausing *)
  let delay_mult = B_options.delay_mult_of_speed v.options.speed in
  let tick_delta = delay_mult * tick_ms in
  let new_time = v.last_tick + tick_delta in
  let ui_msgs = v.ui_msgs in
  v.ui_msgs <- [];

  if cur_time >= new_time then (
    v.last_tick <- cur_time;
    let v, misc_msgs = handle_cycle v in
    v, ui_msgs @ misc_msgs
  )
  else
    v, ui_msgs

let _month_of_time time = (time / month_ticks) mod 12

let get_date v = _month_of_time v.time, v.year

module Action = struct
  type stop = [`Stop of int | `Priority]
  type t =
    | NoAction
    | BuildTrack of Utils.msg
    | BuildFerry of Utils.msg
    | BuildStation of {x: int; y: int; kind: Station.kind; player: int}
    | BuildBridge of Utils.msg * Bridge.t
    | BuildTunnel of Utils.msg * int (* length *)
    | RemoveTrack of Utils.msg
    | ImproveStation of {x:int; y:int; player: int; upgrade: Station.upgrade}
    | SetSpeed of B_options.speed
    | BuildTrain of {engine: Engine.make; cars: Goods.t list; station: int * int; other_station: (int * int) option} 
    | SetStopStation of {train: int; stop: stop; station: int * int}
    | RemoveStop of {train: int; stop: stop}
    | AddStopCar of {train: int; stop: stop; car: Goods.t}
    | RemoveStopCar of {train: int; stop: stop; car: int}
    | RemoveAllStopCars of {train: int; stop: stop}

  let run backend = function
    | BuildTrack {x; y; dir; player} ->
        _build_track backend ~x ~y ~dir ~player
    | BuildFerry {x; y; dir; player} ->
        _build_ferry backend ~x ~y ~dir ~player
    | BuildStation {x; y; kind; player} ->
        _build_station backend ~x ~y kind ~player
    | BuildBridge({x; y; dir; player}, kind) ->
        _build_bridge backend ~x ~y ~dir ~kind ~player
    | BuildTunnel({x; y; dir; player}, length) ->
        _build_tunnel backend ~x ~y ~dir ~player ~length
    | RemoveTrack {x; y; dir; player} ->
        _remove_track backend ~x ~y ~dir ~player
    | ImproveStation {x; y; player; upgrade} ->
        _improve_station backend ~x ~y ~player ~upgrade
    | SetSpeed speed ->
        _set_speed backend speed
    | BuildTrain {engine; cars; station; other_station} ->
        _build_train backend station engine cars other_station
    | RemoveStopCar {train; stop; car} ->
        _remove_stop_car backend ~train ~stop ~car
    | SetStopStation {train; stop; station} ->
        _set_stop_station backend ~train ~stop ~station
    | RemoveStop {train; stop} ->
        _remove_stop backend ~train ~stop
    | RemoveAllStopCars {train; stop} ->
        _remove_all_stop_cars backend ~train ~stop
    | AddStopCar {train; stop; car} ->
        _add_stop_car backend ~train ~stop ~car
    | NoAction -> backend

end


