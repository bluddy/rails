open Containers

module Pic = Engine.Pic

let dir = "./data/covert/"

let load_pics () =
  let load_pic ~transparent s = Pic.img_of_file ~transparent (dir ^ s) in
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
    let ndarray = load_pic ~transparent:true s in
    Hashtbl.replace images (Filename.chop_extension s) ndarray)
  transparent;
  List.iter (fun s ->
    let ndarray = load_pic ~transparent:false s in
    Hashtbl.replace images (Filename.chop_extension s) ndarray)
  nontransparent;
  let cat_files = [
    "CITIES.CAT";
    "FINAL3.CAT";
    "FINAL4.CAT";
  ]
  in
  List.iter (fun file ->
    let image_l = Engine.Cat_file.of_file (dir ^ file) in
    List.iter (fun (filename, img) ->
      Hashtbl.replace images (Filename.chop_extension filename) img)
    image_l)
  cat_files;
  images

module Ndarray = Owl_base_dense_ndarray.Generic
type ndarray = (int, Bigarray.int8_unsigned_elt) Ndarray.t

(* All game resources *)
type t = {
  pics: (string, Pic.ndarray) Hashtbl.t;
}

let load_all () =
  let pics = load_pics () in
  {pics }

