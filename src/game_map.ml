module Ndarray = Owl_base_dense_ndarray.Generic

type tile =
  | Ocean
  | Clear
  | Woods
  | CoalMine
  | Lumber
  | Desert
  | Swamp
  | Hill
  | OilWell
  | SaltMine
  | River
  | Farm
  | Village
  | Stockyard
  | Mountain
  | City
  | BigMountain
  | Harbor

type t = tile array

let int_of_tile = function
  | Ocean -> 1
  | Clear -> 2
  | Woods -> 3
  | Harbor -> 4
  | CoalMine | Lumber -> 5
  | Desert | Swamp -> 6
  | Hill -> 7
  | OilWell | SaltMine -> 8
  | River -> 9
  | Farm -> 0xa
  | Mountain -> 0xb
  | Village | Stockyard -> 0xc
  | City -> 0xe
  | BigMountain -> 0xf

let tile_of_int_default = function
  | 1 -> Ocean
  | 2 -> Clear
  | 3 -> Woods
  | 4 -> Harbor
  | 6 -> Desert
  | 7 -> Hill
  | 9 -> River
  | 0xa -> Farm
  | 0xb -> Mountain
  | 0xc -> Village
  | 0xe -> City
  | 0xf -> BigMountain
  | x -> failwith @@ Printf.sprintf "Illegal basic map value %d" x

let map_height = 192
let map_width = 256

let get_tile map x y = map.(y * map_width + x)

let ndarray_of_file filename =
  let arr = Pic.ndarray_of_file filename in
  (* All maps are 256*192 *)
  let ndarray = Ndarray.get_slice [[0;map_height-1]; [0;map_width-1]] arr in
  ndarray

let of_ndarray ndarray =
  let map = Array.make (map_height * map_width) Ocean in
  let idx = ref 0 in
  for i=0 to map_height-1 do
    for j=0 to map_width-1 do
      let v = Ndarray.get ndarray [|i; j|] in
      let map_v = tile_of_int_default v in
      map.(!idx) <- map_v;
      incr idx;
    done
  done;
  map

let of_file filename =
  ndarray_of_file filename |> of_ndarray

let to_ndarray map =
  let ndarray = Ndarray.empty Int8_unsigned [|map_height; map_width|] in
  let idx = ref 0 in
  for i=0 to map_height-1 do
    for j=0 to map_width-1 do
      let v = map.(!idx) in
      let ndarray_v = int_of_tile v in
      Ndarray.set ndarray [|i; j|] ndarray_v;
      incr idx;
    done
  done;
  ndarray

let to_img map =
  let ndarray = to_ndarray map in
  Pic.img_of_ndarray ndarray





