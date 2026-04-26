open Containers

module Pic = Engine.Pic

let dir = "./data/covert/"

let load_pics () =
  let load_ndarray ~transparent s = Pic.img_of_file ~transparent (dir ^ s) in
  let transparent = [
    "BUGS.PIC";
    "CAMERA.PIC";
    "CHASE.PIC";
    "EQUIP2.PIC";
    "FACES.PIC";
    "FACESF.PIC";
    "SPRITES.PIC";
    "SPRITESF.PIC";
  ]
  in
  let nontransparent = [
    "AD.PIC";
    "AFRICA.PIC";
    "BOARD.PIC";
    "CARS.PIC";
    "CENTRAL.PIC";
    "EQUIP1.PIC";
    "EQUIP1M.PIC";
    "EUROPE.PIC";
    "GENDER.PIC";
    "GUYS2.PIC";
    "GUYS3.PIC";
    "HOTEL.PIC";
    "ICONS.PIC";
    "LABS.PIC";
    "SNEAKIN.PIC";
    "STREET.PIC";
    "TRAINING.PIC";
    "WIRETAP.PIC";
  ] in
  let images = Hashtbl.create 20 in
  List.iter (fun s ->
    let ndarray = load_ndarray ~transparent:true s in
    Hashtbl.replace images (Filename.chop_extension s) ndarray)
  transparent;
  List.iter (fun s ->
    let ndarray = load_ndarray ~transparent:false s in
    Hashtbl.replace images (Filename.chop_extension s) ndarray)
  nontransparent;
  images

module Ndarray = Owl_base_dense_ndarray.Generic
type ndarray = (int, Bigarray.int8_unsigned_elt) Ndarray.t

(* All game resources *)
type t = {
  res_maps: (Region.t * ndarray) list;
  res_pics: (string, Pic.ndarray) Hashtbl.t;
  res_cities: (Region.t * (string * int * int) list) list;
}

let load_all () =
  let res_maps =
    List.map (fun (region,s) -> region, dir ^ s
      |> Tilemap.ndarray_of_file) map_names
  in
  let res_cities = List.map Mapgen.load_city_list Region.regions |> 
    List.combine Region.regions
  in
  let res_pics = load_pics () in
  {res_maps; res_pics; res_cities}

