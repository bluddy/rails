open Containers

type t = (Train.t, Utils.Vector.rw) Utils.Vector.t
[@@deriving yojson]

let empty () = Utils.Vector.create ()

let add v train =
  CCVector.push v train; v

let delete v idx =
  CCVector.remove_and_shift v idx; v

let get v idx = CCVector.get v idx

let update v idx f =
  let train = get v idx in
  CCVector.set v idx (f train);
  v

let size v = CCVector.size v

let last v =
  let size = size v in
  get v (size - 1)

