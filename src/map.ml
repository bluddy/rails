open Containers
module Ndarray = Owl_base_dense_ndarray.Generic

type tile =
  | Ocean
  | Clear
  | Forest
  | CoalMine
  | Lumber
  | Desert
  | Hill
  | OilWell
  | SaltMine
  | River
  | Farm
  | Village
  | Stockyard
  | Mountains
  | City
  | BigMountains

let int_of_tile = function
  | Ocean -> 1
  | Clear -> 2
  | Forest -> 3
  | CoalMine
  | Lumber -> 5
  | Desert -> 6
  | Hill -> 7
  | OilWell
  | SaltMine -> 8
  | River -> 9
  | Farm -> 0xa
  | Village
  | Stockyard -> 0xc
  | Mountains -> 0xb
  | City -> 0xe
  | BigMountains -> 0xf

let tile_of_int_default = function
  | 1 -> Ocean
  | 2 -> Clear
  | 3 -> Forest
  | 6 -> Desert
  | 9 -> River
  | 0xa -> Farm
  | x -> failwith @@ Printf.sprintf "Illegal basic map value %d" x

let map_of_file filename =
  let arr = Pic.bigarray_of_file filename in
  (* All maps are 256*192 *)
  let bigarr = Ndarray.get_slice [[0;192]; [0;256]] bigarr in
  bigarr




