open! Containers
open! Utils.Infix
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Hashtbl = Utils.Hashtbl
module Vector = Utils.Vector
module C = Constants


module Id = Int_id.Make(struct end)

(* Note: very important to keep the tile_idx updated all the time *)

type t = {
  trains: Train.t Vector.vector;
  tile_idx: (Utils.loc, Id.t list) Hashtbl.t;  (* Tiles of train locations. *)
}
[@@deriving yojson]

let empty () = { trains=Vector.create (); tile_idx=Hashtbl.create 10 }

let _calc_train_loc (train:Train.t) = train.x / C.tile_dim, train.y / C.tile_dim

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

  (* Only for viewing, *not for updating* *)
let get v idx =
  Vector.get v.trains @@ Id.to_int idx

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
  let loc1 = _calc_train_loc train in
  let train = f train in
  let loc2 = _calc_train_loc train in
  if not @@ Utils.equal_loc loc1 loc2 then (
    _remove_train_loc v loc1 idx;
    _add_train_loc v loc2 idx;
  );
  train

let update v idx f =
  let train1 = get v idx in
  let train2 = _with_update_loc v idx train1 f in
  if train1 =!= train2 then (
    Vector.set v.trains (Id.to_int idx) train2
  );
  v

let size v = Vector.size v.trains

let get_last v =
  let size = size v in
  get v @@ Id.of_int @@ size - 1

  (* Read-only *)
let iter f (v:t) = Vector.iter f v.trains

  (* Read-only *)
let iteri f (v:t) = Vector.iteri f v.trains

  (* Read-only *)
let fold f (v:t) ~init = Vector.fold f init v.trains

  (* Read-only *)
let foldi f (v:t) ~init = Vector.foldi f init v.trains

let mapi_in_place f v = 
  Vector.mapi_in_place (fun i train ->
    _with_update_loc v (Id.of_int i) train (f i)
  )
    v.trains

  (* Return the index of a train that matches *)
  (* read-only *)
let find_ret_index f (v:t) =
  let exception Stop of int in
  try
    iteri (fun i x -> if f x then raise (Stop i)) v;
    None
  with Stop i -> Some (Id.of_int i)

  (* Return indices of trains at location *)
let get_at_loc loc (v:t) =
  Hashtbl.find_opt v.tile_idx loc
  |> Option.get_or ~default:[]

