open! Containers
open! Utils.Infix
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Hashtbl = Utils.Hashtbl
module Vector = Utils.Vector
module C = Constants
open Utils.Infix

module IntMap = Utils.IntMap
module Id = Train.Id
module IdMap = Train.IdMap

(* It's very important to keep the tile_idx updated all the time.
   We do this using r/w and r/o access functions
 *)

type rw = Train.rw
type ro = Train.ro

type t = {
  trains: (rw Train.t) Vector.vector;
  tile_idx: (Utils.loc, Id.t list) Hashtbl.t;  (* Tiles of train locations. *)
  p_to_ids: Id.t list IntMap.t; (* p = priority *)
  id_to_p: int IdMap.t (* p = priority *)
}
[@@deriving yojson]

let empty () = {
  trains=Vector.create ();
  tile_idx=Hashtbl.create 10;
  p_to_ids=IntMap.empty;
  id_to_p=IdMap.empty;
}

let _calc_train_loc (train:'a Train.t) = train.x / C.tile_dim, train.y / C.tile_dim

let _add_train_loc loc train_id v =
  Hashtbl.update v.tile_idx ~k:loc ~f:(fun _k -> function
    | None -> Some [train_id]
    | Some ids -> Some (train_id::ids))

let _remove_train_loc loc train_id v =
  Hashtbl.update v.tile_idx ~k:loc ~f:(fun _k -> function
    | Some ids ->
      begin match List.filter (fun id -> not @@ Id.equal id train_id) ids with
      | [] -> None
      | l -> Some l
      end
    | _ -> None
  )

let _freeze_all (trains: (rw Train.t) Vector.vector) : ((ro Train.t) Vector.vector) = Obj.magic trains

(* R/W for internal use only *)
let _get v idx =
  Vector.get v.trains @@ Id.to_int idx

  (* Only for viewing, *not for updating* *)
let get idx v : ro Train.t =
  Vector.get (_freeze_all v.trains) @@ Id.to_int idx

module Priorities = struct
    (* Deal with priority data structure *)
  let add priority train_id id_to_p p_to_ids =
    let p_to_ids = IntMap.update priority
      (function 
        | Some l -> Some (train_id::l)
        | None -> Some [train_id])
      p_to_ids
    in
    let id_to_p = IdMap.add train_id priority id_to_p in
    id_to_p, p_to_ids

  let remove train_id id_to_p p_to_ids =
    let priority = IdMap.find train_id id_to_p in
    let p_to_ids = IntMap.update priority
      (function 
        | Some l ->
            let l = List.filter (fun x -> not @@ Id.equal x train_id) l in
            if List.is_empty l then None else Some l
        | None -> None)
      p_to_ids
    in
    let id_to_p = IdMap.remove train_id id_to_p in
    id_to_p, p_to_ids

  let find train_id v = IdMap.find train_id v

  (* fold on the int -> id list structure *)
  let fold f ~init v =
    IntMap.fold (fun _prio train_list acc ->
      (* don't care about priority, it's just for order *)
      List.fold_left (fun acc train_id -> f train_id acc)
        acc
        train_list)
      v
      init

end

let add train v =
  Vector.push v.trains train;
  let train_id = Id.of_int @@ Vector.size v.trains - 1 in
  let loc = _calc_train_loc train in
  _add_train_loc loc train_id v;
  let priority = Train.calc_priority train in
  let id_to_p, p_to_ids = Priorities.add priority train_id v.id_to_p v.p_to_ids in
  {v with p_to_ids; id_to_p}

let delete train_id v =
  let train = get train_id v in
  let loc = _calc_train_loc train in
  _remove_train_loc loc train_id v ;
  Vector.remove_and_shift v.trains @@ Id.to_int train_id;
  let id_to_p, p_to_ids = Priorities.remove train_id v.id_to_p v.p_to_ids in
  {v with p_to_ids; id_to_p}

let _with_update_loc v idx train f =
  (* Any r/w action on trains needs to update their positions in the index *)
  let loc1 = _calc_train_loc train in
  let train = f train in
  let loc2 = _calc_train_loc train in
  if not @@ Utils.equal_loc loc1 loc2 then (
    _remove_train_loc loc1 idx v;
    _add_train_loc loc2 idx v;
  );
  train

let _with_update_loc_pair idx train v f =
  (* Same as above, returning a value too *)
  let loc1 = _calc_train_loc train in
  let x, train = f train in
  let loc2 = _calc_train_loc train in
  if not @@ Utils.equal_loc loc1 loc2 then (
    _remove_train_loc loc1 idx v;
    _add_train_loc loc2 idx v;
  );
  x, train

  (* Update a train. R/W *)
let update idx v f =
  let train = _get v idx in
  let train' = _with_update_loc v idx train f in
  if train =!= train' then (
    Vector.set v.trains (Id.to_int idx) train'
  );
  v

  (* Update a train. R/W *)
let update_get_val idx v f =
  let train = _get v idx in
  let x, train' = _with_update_loc_pair idx train v f in
  if train =!= train' then (
    Vector.set v.trains (Id.to_int idx) train'
  );
  x, v

let size v = Vector.size v.trains

let get_last v : ro Train.t =
  let size = size v in
  get (Id.of_int @@ size - 1) v

let iter (f:ro Train.t -> unit) (v:t) = Vector.iter f @@ _freeze_all v.trains

let iteri (f: Id.t -> ro Train.t -> unit) (v:t) = Vector.iteri (fun i train -> f (Id.of_int i) train) @@ _freeze_all v.trains

let fold (f: 'a -> ro Train.t -> 'a) (v:t) ~init = Vector.fold f init @@ _freeze_all v.trains

let sum f v = fold (fun acc train -> acc + f train) ~init:0 v

let sum_money f v = fold (fun acc train -> Money.(acc + f train)) ~init:Money.zero v

let foldi (f: Id.t -> 'a -> ro Train.t -> 'a) (v:t) ~init =
  Vector.foldi (fun i acc train -> f (Id.of_int i) acc train) init @@ _freeze_all v.trains

(* R/W *)
let mapi_in_place f v = 
  Vector.mapi_in_place (fun i train ->
    let id = Id.of_int i in
    _with_update_loc v id train (f id)
  )
  v.trains;
  v

(* R/W *)
let fold_mapi_in_place f v ~init =
  Vector.fold_mapi_in_place (fun i acc train ->
    let id = Id.of_int i in
    _with_update_loc_pair id train v (f id acc))
    ~init
    v.trains

  (* Similar to fold-map, but goes by priority (0->up). Also, we make sure
     to update the priority structure as needed per train for the next iteration
   *)
let fold_mapi_by_priority f v ~init =
  let acc, v, id_to_p, p_to_ids =
    Priorities.fold (fun train_id (acc, v, id_to_p, p_to_ids) ->
      let (p, acc), v =
        update_get_val train_id v (fun train ->
           let acc, train = f train_id acc train in
           (Train.calc_priority train, acc), train) in
      (* Update priority if needed for next loop *)
      let saved_p = Priorities.find train_id id_to_p in
      let id_to_p, p_to_ids =
        if p <> saved_p then
          Priorities.add p train_id id_to_p p_to_ids
        else
          id_to_p, p_to_ids
      in
      acc, v, id_to_p, p_to_ids)
      ~init:(init, v, v.id_to_p, v.p_to_ids)
      v.p_to_ids
  in
  acc, [%up {v with id_to_p; p_to_ids}]

  (* Return the index of a train that matches *)
let find_ret_index (f:ro Train.t -> bool) (v:t) =
  let exception Stop of Id.t in
  try
    iteri (fun i x -> if f x then raise (Stop i)) v;
    None
  with Stop i -> Some i

  (* Return indices of trains at a given location *)
let get_at_loc loc (v:t) =
  Hashtbl.find_opt v.tile_idx loc |> Option.get_or ~default:[]

let total_engine_value (v:t) = sum_money Train.get_engine_cost v

let total_car_value (v:t) = Money.(C.car_cost * (sum Train.num_of_cars v))

let total_maintenance (v:t) = sum_money Train.full_maintenance_cost v

let clear_priority_shipment v =
  (* Clear priority shipment holding for the given players *)
  let open Train in
  Vector.map_in_place (fun train ->
    if Train.holds_priority_shipment train then
      Train.set_priority_shipment false train
    else train)
    v.trains;
  v

let remove_goods_in_all_trains remove_goods v =
  Vector.map_in_place (fun train ->
    Train.remove_goods remove_goods train)
  v.trains;
  v

let subrange start ~num v =
  let start = Id.to_int start in
  if start < size v - 1 || num <= 0 then Iter.empty, `First, `Last else
  let end_ = min (start + num - 1) (size v - 1) in
  let first = if start = 0 then `First else `NotFirst in
  let last = if end_ >= size v - 1 then `Last else `NotLast in
  Iter.(start -- end_), first, last

let first_train _ = Train.Id.of_int 0

let find_trains_in_range ~x ~y ~range v =
  (* Trains are always per-player so no need to worry *)
  Iter.fold (fun acc i ->
     Iter.fold (fun acc j ->
        let trains = Hashtbl.find v.tile_idx (j, i) in
        trains @ acc)
       acc
       Iter.((x-range) -- (x+range)))
    []
    Iter.((y-range) -- (y+range))

