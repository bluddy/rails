open! Containers
open! Utils.Infix
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Hashtbl = Utils.Hashtbl
module Vector = Utils.Vector
module C = Constants
open Utils.Infix

module IntMap = Utils.IntMap
module Id = Train.Id

(* It's very important to keep the tile_idx updated all the time.
   We do this using r/w and r/o access functions
 *)

type rw = Train.rw
type ro = Train.ro

type t = {
  trains: (rw Train.t) Vector.vector;
  tile_idx: (Utils.loc, Id.t list) Hashtbl.t;  (* Tiles of train locations. *)
  priorities: Id.t list IntMap.t;
}
[@@deriving yojson]

let empty () = {
  trains=Vector.create ();
  tile_idx=Hashtbl.create 10;
  priorities=IntMap.empty;
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
let add priority train_id priorities =
  let priorities = IntMap.update priority
    (function 
      | Some l -> Some (train_id::l)
      | None -> Some [train_id])
    priorities
  in
  priorities

let remove priority train_id priorities =
  let priorities = IntMap.update priority
    (function 
      | Some l ->
          let l = List.filter (fun x -> not @@ Id.equal x train_id) l in
          if List.is_empty l then None else Some l
      | None -> None)
    priorities
  in
  priorities
end

let add train v =
  Vector.push v.trains train;
  let train_id = Id.of_int @@ Vector.size v.trains - 1 in
  let loc = _calc_train_loc train in
  _add_train_loc loc train_id v;
  let priority = Train.calc_priority train in
  let priorities = Priorities.add priority train_id v.priorities in
  {v with priorities}

let delete train_id v =
  (* We need the train for the loc *)
  let train = get train_id v in
  let loc = _calc_train_loc train in
  _remove_train_loc loc train_id v ;
  Vector.remove_and_shift v.trains @@ Id.to_int train_id;
  let priority = Train.calc_priority train in
  let priorities = Priorities.remove priority train_id v.priorities in
  {v with priorities}

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
  (* Any r/w action on trains needs to update their positions in the index *)
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
  let train1 = _get v idx in
  let train2 = _with_update_loc v idx train1 f in
  if train1 =!= train2 then (
    Vector.set v.trains (Id.to_int idx) train2
  );
  v

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

