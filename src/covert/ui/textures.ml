open Containers
module Ndarray = Owl_base_dense_ndarray.Generic

module R = Engine.Renderer
module C = Constants
module Dir = Engine.Dir

let slice win img x y w h =
  let x2, y2 = x + w, y + h in
  Ndarray.get_slice [[y; y2]; [x; x2]] img |> R.Texture.make win

module Cities = struct
  let add win res =
    let h = Hashtbl.create 10 in
    City.iter_all (fun city ->
      let city_s = City.show city |> String.uppercase_ascii |> String.take 8 in
      let img = Hashtbl.find res.Resources.pics city_s in
      let tex = R.Texture.make win img in
      Hashtbl.replace h city tex
    );
    h
end

module Car_frames = struct
  let add win res =
    let h = Hashtbl.create 10 in
    let img = Hashtbl.find res.Resources.pics "STREET" in
    let handle img_num =
      let x = (img_num / 4) * 125 in
      let y = (img_num mod 4) * 47 in
      let p = slice win img x y 117 25 in
      Hashtbl.replace h img_num p
    in
    Iter.(iter handle (0 -- 7));
    h
end

module Face_parts = struct

  let add_all win res =
    let hash = Hashtbl.create 10 in
    let face_img = Hashtbl.find res.Resources.pics "FACES" in
    let facef_img = Hashtbl.find res.Resources.pics "FACESF" in

    let add_faces img const =
      let handle i =
        let x = (i mod 8) * 32 + 1 in
        let y = i/8 * 35 + 1 in
        let tex = slice win img x y 30 33 in
        Hashtbl.replace hash (const i) tex
      in
      Iter.iter handle Iter.(0 -- 39)
    in
    add_faces face_img (fun x -> `Male_face x);
    add_faces facef_img (fun x -> `Female_face x);

    let add_neck img const =
      let handle i =
        let tex = slice win img 257 (i * 20) 44 18 in
        Hashtbl.replace hash (const i) tex
      in
      Iter.iter handle Iter.(0 -- 3)
    in
    add_neck face_img (fun x -> `Male_neck x);
    add_neck face_img (fun x -> `Female_neck x);
    hash
end

module Clue_icons = struct
  let add_all win res =
    let hash = Hashtbl.create 10 in
    let face_img = Hashtbl.find res.Resources.pics "FACES" in

    List.mapi (fun i means ->
      let x, y = i*32 + 96, 174 in
      let tex = slice win face_img x y 32 26 in
      Hashtbl.replace hash (`Icon means) tex)
    Clue.means_list
end

module Images = struct
  type t = [
    | `Ad
    | `Africa
    | `Central
    | `Europe
    | `Board
    | `Cars
    | `Equip1F
    | `Equip1M
    | `Gender
    | `Hotel
    | `MPS_labs
    | `Sneak_in
    | `Training
  ]
  let names = [
    `Ad, "AD";
    `Africa, "AFRICA";
    `Central, "CENTRAL";
    `Europe, "EUROPE";
    `Board, "BOARD";
    `Cars, "CARS";
    `Equip1F, "EQUIP1";
    `Equip1M, "EQUIP1M";
    `Gender, "GENDER";
    `Hotel, "HOTEL";
    `MPS_labs, "LABS";
    `Sneak_in, "SNEAKIN";
    `Training, "TRAINING";
  ]
  let add win res =
    let h = Hashtbl.create 10 in
    List.iter (fun (id, str) ->
      let tex = Hashtbl.find res.Resources.pics str |> R.Texture.make win in
      Hashtbl.replace h id tex)
      names;

    let face_img = Hashtbl.find res.Resources.pics "FACES" in
    h
end

type t = {
  pixel: R.Texture.t; (* white pixel *)
  cities: (City.t, R.Texture.t) Hashtbl.t;
  car_frames: (int, R.Texture.t) Hashtbl.t;
  images: (Images.t, R.Texture.t) Hashtbl.t;
}

let of_resources win res =
  let pixel = R.Texture.make win Engine.Pic.white_pixel in
  let cities = Cities.add win res in
  let car_frames = Car_frames.add win res in
  let images = Images.add win res in
  {
    pixel;
    cities;
    car_frames;
    images;
  }

