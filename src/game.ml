open Containers

let data_dir = "./data/"

let map_names =
  let open Gmap in
  [
    EastUS, "EASTUS.PIC";
    WestUS, "WESTUS.PIC";
    Britain, "BRITAIN.PIC";
    Europe, "EUROPE.PIC"
  ]

let load_pics () =
  let load_ndarray s = Pic.ndarray_of_file @@ data_dir ^ s ^ ".PIC" in
  let images = Hashtbl.create 20 in
  let filenames = [
    "SPRITES"; "TRACKS"; "STATION"; "FACES";
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

(* The actual game state *)
type t = {
  map : Gmap.t;
}

(* All game resources *)
type resources = {
  maps: (Gmap.area * Gmap.t) list;
  pics: (string, Pic.ndarray) Hashtbl.t;
  fonts: Font.t array;
}

type state = {
  game: t;
  screen: Screen.t;
  resources: resources;
}

let run ?(view=Screen.MapGen) ?(area=Gmap.WestUS) () =
  Printf.printf "Loading resources...";
  let maps = List.map (fun (x,s) -> x, "./data/" ^ s |> Gmap.of_file) map_names in
  let pics = load_pics () in
  let fonts = Font.load_all () in
  let resources = {maps; pics; fonts} in

  let screen = Screen.make view in

  let map = List.assoc ~eq:(Stdlib.(=)) area maps in
  let v = {map} in

  let state = {game=v; screen; resources} in
  Printf.printf "done.\n"





