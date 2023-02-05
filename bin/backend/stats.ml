open Containers

type t = {
  mutable dist_traveled: int;
} [@@ deriving yojson]

let default = {
  dist_traveled=0;
}
