open! Containers

include String

let remove_nulls s =
  filter (fun c -> Char.(<>) c '\000') s

