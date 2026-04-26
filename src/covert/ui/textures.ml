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

module DrivingFrames = struct
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
  images: (Images.t, R.Texture.t) Hashtbl.t;
}

let of_resources win res =
  let pixel = R.Texture.make win Engine.Pic.white_pixel in
  let images = Images.add win res in
  {
    pixel;
    images;
  }


