open Containers

type t = (Train.t, Utils.Vector.rw) Utils.Vector.t
[@@deriving sexp]

let empty () = Utils.Vector.create ()

let add v train =
  CCVector.push v train; v

let delete v idx =
  CCVector.remove_and_shift v idx; v

let get v idx = CCVector.get v idx

let size v = CCVector.size v

let last v =
  let size = size v in
  get v (size - 1)

