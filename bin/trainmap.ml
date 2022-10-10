open Containers

type t = (Train.t, Utils.Vector.rw) Utils.Vector.t
[@@deriving sexp]

let empty () = Utils.Vector.create ()

let add v train =
  CCVector.push v train; v

let delete v idx =
  CCVector.remove_and_shift v idx; v
