open Containers

let dir = "./data/"

let map_names =
  let open Gmap in
  [
    EastUS, "EASTUS.PIC";
    WestUS, "WESTUS.PIC";
    Britain, "BRITAIN.PIC";
    Europe, "EUROPE.PIC"
  ]

let load_pics () =
  let load_ndarray s = Pic.img_of_file @@ dir ^ s in
  let images = Hashtbl.create 20 in
  let filenames = [
    (* E and C versions are for England and Europe *)
    "SPRITES.PIC"; "CSPRITES.PIC"; "ESPRITES.PIC"; "SPRITES_extra.png";
    "TRACKS.PIC"; "STATION.PIC"; "FACES.PIC"; "BRITAIN.PIC";
    "TITLE.PIC";
    "LOGO.PIC"; "LABS.PIC"; "CREDITS2.PIC"; "ADVERT.PIC";
    "DIFFS.PIC"; "DIFFSP.PIC"; "COUNCIL.PIC";
    "PAGE0.PIC"; "PAGE1.PIC"; "PAGE2.PIC"; "PAGE3.PIC"; "PAGE4.PIC";
    "PAGE5.PIC"; "PAGE6.PIC"; "PAGE7.PIC"; "PAGE8.PIC"; "PAGE9.PIC";
    "LOCOS.PIC"; "CLOCOS.PIC"; "ELOCOS.PIC";
    "ELOCOS0.PIC"; "ELOCOS1.PIC"; "ELOCOS2.PIC"; "ELOCOS3.PIC"; "ELOCOSM.PIC";
    "LOCOS0.PIC"; "LOCOS1.PIC"; "LOCOS2.PIC"; "LOCOSM.PIC";
  ]
  in
  List.iter (fun s ->
    let ndarray = load_ndarray s in
    Hashtbl.replace images (Filename.chop_extension s) ndarray)
  filenames;
  images

(* All game resources *)
type t = {
  res_maps: (Gmap.area * Gmap.t) list;
  res_pics: (string, Pic.ndarray) Hashtbl.t;
  res_cities: (Gmap.area * (string * int * int) list) list;
}

let load_all ~seed =
  let res_maps =
    List.map (fun (area,s) -> area, dir ^ s
      |> Gmap.of_file ~area ~seed) map_names
  in
  let res_cities = List.map Mapgen.load_city_list Gmap.areas |> 
    List.combine Gmap.areas
  in
  let res_pics = load_pics () in
  {res_maps; res_pics; res_cities}

open Dir
let track_dirs =
  [[Up]; [UpRight]; [Right]; [DownRight]; [Down]; [DownLeft]; [Left]; [UpLeft];
  [Up; DownRight]; [Up; Down]; [Up; DownLeft]; [Down; UpRight]; [UpRight; DownLeft]; [Left; UpRight];
    [Right; DownLeft]; [Left; Right];
  [Right; UpLeft]; [Left; DownRight]; [UpLeft; DownRight]; [Down; UpLeft]; [Up; Down; DownRight];
    [Up; Down; DownLeft]; [Up; Down; UpRight]; [Up; Down; UpLeft]; [Left; Right; UpRight];
    [Left; Right; DownRight]; [Left; Right; UpLeft]; [Left; Right; DownLeft]; [Down; DownLeft; UpRight];
    [Left; DownLeft; UpRight]; [Up; UpLeft; DownRight]; [Left; UpLeft; DownRight];
  [Down; UpLeft; DownRight]; [Right; UpLeft; DownRight]; [Up; DownLeft; UpRight]; [Right; UpRight; DownLeft]]

  (* Only the computer can make these *)
let illegal_track =
  [[Up; Down; Left; Right]; [UpRight; DownRight; DownLeft; UpLeft]]

let track_turns =
  [[Up; Right]; [Down; Right]; [Down; Left]; [Up; Left]; [UpRight; DownRight]; [DownLeft; DownRight];
    [UpLeft; DownLeft]; [UpLeft; UpRight]]

  (* Dirs for non-track stuff *)
let special_dirs =
  [[Dir.Up;Down]; [UpRight; DownLeft]; [Left;Right]; [UpLeft; DownRight]]

