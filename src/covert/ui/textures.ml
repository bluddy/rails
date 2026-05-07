open Containers
module Ndarray = Owl_base_dense_ndarray.Generic

module R = Engine.Renderer
module C = Constants
module Dir = Engine.Dir

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
      let x1 = (img_num / 4) * 125 in
      let y1 = (img_num mod 4) * 47 in
      let x2, y2 = x1 + 117, y1 + 25 in
      let p = Ndarray.get_slice [[y1; y2]; [x1; x2]] img
        |> R.Texture.make win
      in
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
        let x1 = (i mod 8) * 32 + 1 in
        let y1 = i/8 * 35 + 1 in
        let x2, y2 = x1 + 30, y1 + 33 in
        let tex = Ndarray.get_slice [[y1; y2]; [x1; x2]] img |> R.Texture.make win in
        Hashtbl.replace hash (const i) tex
      in
      Iter.iter handle Iter.(0 -- 39)
    in
    add_faces face_img (fun x -> `Male_face x);
    add_faces facef_img (fun x -> `Female_face x);

    let add_neck img const =
      let handle i =
        let x1 = 257 in
        let y1 = i * 20 in
        let w, h = 44, 18 in
        let x2, y2 = x1 + w, y1 + h in
        let tex = Ndarray.get_slice [[y1; y2]; [x1; x2]] img |> R.Texture.make win in
        Hashtbl.replace hash (const i) tex
      in
      Iter.iter handle Iter.(0 -- 3)
    in
    add_neck face_img (fun x -> `Male_neck x);
    add_neck face_img (fun x -> `Female_neck x);
    hash
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

