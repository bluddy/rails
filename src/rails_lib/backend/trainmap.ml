open Containers
open Utils.Infix
module Vector = Utils.Vector

type t = Train.t Vector.vector
[@@deriving yojson]

let empty () = Vector.create ()

let add v train =
  Vector.push v train; v

let delete v idx =
  Vector.remove_and_shift v idx; v

let get v idx = Vector.get v idx

let update v idx f =
  let train = get v idx in
  Vector.set v idx (f train);
  v

let size v = Vector.size v

let last v =
  let size = size v in
  get v (size - 1)

let iter f (v:t) = Vector.iter f v

let iteri f (v:t) = Vector.iteri f v

let fold f (v:t) ~init = Vector.fold f init v

let foldi f (v:t) ~init = Vector.foldi f init v

let mapi_in_place f v = 
  Vector.mapi_in_place f v

