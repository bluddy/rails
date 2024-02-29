open! Containers
open! Utils.Infix
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Hashtbl = Utils.Hashtbl
module Vector = Utils.Vector
module C = Constants
open Utils.Infix


module Id = Int_id.Make(struct end)

(* It's very important to keep the tile_idx updated all the time.
   We do this using r/w and r/o access functions
 *)

type rw = Train.rw
type ro = Train.ro

type t = {
  trains: (rw Train.t) Vector.vector;
  tile_idx: (Utils.loc, Id.t list) Hashtbl.t;  (* Tiles of train locations. *)
}
[@@deriving yojson]

let empty () = { trains=Vector.create (); tile_idx=Hashtbl.create 10 }

let _calc_train_loc (train:'a Train.t) = train.x / C.tile_dim, train.y / C.tile_dim

let _add_train_loc v loc train_id =
  Hashtbl.update v.tile_idx ~k:loc ~f:(fun _k -> function
    | None -> Some [train_id]
    | Some ids -> Some (train_id::ids))

let _remove_train_loc v loc train_id =
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
let get v idx : ro Train.t =
  Vector.get (_freeze_all v.trains) @@ Id.to_int idx

let add v train =
  Vector.push v.trains train;
  let train_id = Id.of_int @@ Vector.size v.trains - 1 in
  let loc = _calc_train_loc train in
  _add_train_loc v loc train_id;
  v

let delete v idx =
  (* We need the train for the loc *)
  let loc = get v idx |> _calc_train_loc in
  _remove_train_loc v loc idx;
  Vector.remove_and_shift v.trains @@ Id.to_int idx;
  v

let _with_update_loc v idx train f =
  (* Any r/w action on trains needs to update their positions in the index *)
  let loc1 = _calc_train_loc train in
  let train = f train in
  let loc2 = _calc_train_loc train in
  if not @@ Utils.equal_loc loc1 loc2 then (
    _remove_train_loc v loc1 idx;
    _add_train_loc v loc2 idx;
  );
  train

let _with_update_loc_pair v idx train f =
  (* Any r/w action on trains needs to update their positions in the index *)
  let loc1 = _calc_train_loc train in
  let train, x = f train in
  let loc2 = _calc_train_loc train in
  if not @@ Utils.equal_loc loc1 loc2 then (
    _remove_train_loc v loc1 idx;
    _add_train_loc v loc2 idx;
  );
  train, x

  (* Update a train. R/W *)
let update v idx f =
  let train1 = _get v idx in
  let train2 = _with_update_loc v idx train1 f in
  if train1 =!= train2 then (
    Vector.set v.trains (Id.to_int idx) train2
  );
  v

let size v = Vector.size v.trains

let get_last v : ro Train.t =
  let size = size v in
  get v @@ Id.of_int @@ size - 1

let iter (f:ro Train.t -> unit) (v:t) = Vector.iter f @@ _freeze_all v.trains

let iteri (f: int -> ro Train.t -> unit) (v:t) = Vector.iteri f @@ _freeze_all v.trains

let fold (f: 'a -> ro Train.t -> 'a) (v:t) ~init = Vector.fold f init @@ _freeze_all v.trains

let foldi (f: int -> 'a -> ro Train.t -> 'a) (v:t) ~init = Vector.foldi f init @@ _freeze_all v.trains

(* R/W *)
let mapi_in_place f v = 
  Vector.mapi_in_place (fun i train ->
    _with_update_loc v (Id.of_int i) train (f i)
  )
  v.trains;
  v

(* R/W *)
let fold_mapi_in_place f v ~init =
  Vector.fold_mapi_in_place (fun i acc train ->
    _with_update_loc_pair v (Id.of_int i) train (f i acc))
    init v.trains

  (* Return the index of a train that matches *)
let find_ret_index (f:ro Train.t -> bool) (v:t) =
  let exception Stop of int in
  try
    iteri (fun i x -> if f x then raise (Stop i)) v;
    None
  with Stop i -> Some (Id.of_int i)

  (* Return indices of trains at a given location *)
let get_at_loc loc (v:t) =
  Hashtbl.find_opt v.tile_idx loc |> Option.get_or ~default:[]

