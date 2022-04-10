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
  let load_ndarray s = Pic.img_of_file @@ dir ^ s ^ ".PIC" in
  let images = Hashtbl.create 20 in
  let filenames = [
    "SPRITES"; "TRACKS"; "STATION"; "FACES"; "BRITAIN";
    "TITLE";
    "LOGO"; "LABS"; "CREDITS2"; "ADVERT";
    "DIFFS"; "DIFFSP"; "COUNCIL";
    "PAGE0"; "PAGE1"; "PAGE2"; "PAGE3"; "PAGE4"; "PAGE5"; "PAGE6"; "PAGE7"; "PAGE8"; "PAGE9";
    "ELOCOS"; "ELOCOS0"; "ELOCOS1"; "ELOCOS2"; "ELOCOS3"; "ELOCOSM";
    "LOCOS"; "LOCOS0"; "LOCOS1"; "LOCOS2"; "LOCOSM";
  ]
  in
  List.iter (fun s ->
    let ndarray = load_ndarray s in
    Hashtbl.replace images s ndarray)
  filenames;
  images

(* All game resources *)
type t = {
  res_maps: (Gmap.area * Gmap.t) list;
  res_pics: (string, Pic.ndarray) Hashtbl.t;
  res_cities: (Gmap.area * Gmap.city list) list;
}

let load_all ~random_seed =
  let res_maps =
    List.map (fun (x,s) -> x, dir ^ s
      |> Gmap.of_file ~random_seed) map_names
  in
  let res_cities = List.map Mapgen.load_city_list Gmap.areas |> 
    List.combine Gmap.areas
  in
  let res_pics = load_pics () in
  {res_maps; res_pics; res_cities}

