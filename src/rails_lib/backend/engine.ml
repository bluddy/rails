open Containers
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type make =
  (* One to one mapping between Engine.make and Engine.t *)
  (* US engines *)
  | Grasshopper
  | Norris
  | American
  | Mogul
  | TenWheeler
  | Consolidation
  | Pacific
  | Mikado
  | Mallet
  | FSeriesDiesel
  | GPSeriesDiesel (* seems missing sprite? *)

  (* British/Euro Engines *)
  | Planet
  | Patentee
  | IronDuke
  | DxGoods
  | Stirling
  | MidlandSpinner
  | WebbCompound
  | ClaudHamilton
  | A1Class
  | A4Class
  | ClassCrocodile
  | ClassE18
  | R242A1
  | V200BB
  | BoBoBo
  | TGV
  [@@deriving yojson, eq, show]

type _type =
  SteamSmall | SteamBig | Diesel
  [@@deriving yojson, show]

type t = {
  make: make;
  _type: _type;
  name: string;
  max_speed: int;   (* in 5s *)
  horsepower: int; (* in 500s *)
  price: int; (* In 1000s *)
  year: int;
} [@@deriving yojson, show]

let has_steam v = match v._type with
  | SteamSmall | SteamBig -> true
  | Diesel -> false

let make make _type name max_speed horsepower price year = {
  make;
  _type;
  name;
  max_speed=max_speed/5; (* To fit the original *)
  horsepower=horsepower/500;
  price=price/1000;
  year }

let us_engines = 
  (* US *)
  [
    make Grasshopper SteamSmall "Grasshopper" 20 500 10000 1820;
    make Norris SteamSmall "4-2-0 Norris" 30 1000 20000 1833;
    make American SteamSmall "4-4-0 American" 40 1500 30000 1848;
    make Mogul SteamBig "2-6-0 Mogul" 25 2000 30000 1851;
    make TenWheeler SteamBig "4-6-0 Ten Wheeler" 45 2000 40000 1868;
    make Consolidation SteamBig "2-8-0 Consolidation" 40 2500 40000 1877;
    make Pacific SteamBig "4-6-2 Pacific" 60 3500 60000 1892;
    make Mikado SteamBig "2-8-2 Mikado" 45 3500 50000 1903;
    make Mallet Diesel "2-6-6-2 Mallet" 50 4500 70000 1911;
    make FSeriesDiesel Diesel "'F' Series Diesel" 70 3500 75000 1916;
    make GPSeriesDiesel Diesel "'GP' Series Diesel" 60 4000 75000 1930;
  ]

let en_engines =
  (* England *)
  [
    make Planet SteamSmall "2-2-0 Planet" 20 500 10000 1820;
    make Patentee SteamSmall "2-2-2 Patentee" 30 1000 20000 1835;
    make IronDuke SteamSmall "4-2-2 Iron Duke" 40 1500 30000 1845;
    make DxGoods SteamBig "0-6-0 DX Goods" 25 2500 30000 1855;
    make Stirling SteamBig "4-2-2 Stirling" 45 2000 40000 1870;
    make MidlandSpinner SteamBig "4-2-2 Spinner" 50 2500 50000 1880;
    make WebbCompound SteamBig "0-8-0 Compound" 40 3000 50000 1890;
    make ClaudHamilton SteamBig "4-4-0 Hamilton" 60 2500 60000 1900;
    make A1Class SteamBig "4-6-2 Gresley" 50 4000 60000 1920;
    make A4Class SteamBig "4-6-2 Class A4" 70 3000 70000 1930; (* EN: 3000 HP? *)
  ]

let eu_engines =
  (* Europe *)
  [
    make WebbCompound SteamSmall "0-8-0 Compound" 40 3000 50000 1880;
    make ClaudHamilton SteamSmall "4-4-0 Hamilton" 60 2500 60000 1890;
    make A1Class SteamSmall "4-6-2 Gresley" 45 4000 60000 1905;
    make A4Class SteamBig "4-6-2 Class A4" 70 3500 70000 1915; (* EN: 3000 HP? *)
    make ClassCrocodile Diesel "6/6 Crocodile" 40 5000 50000 1925;
    make ClassE18 Diesel "Class E18 1-D-1" 80 5000 80000 1935;
    make R242A1 SteamBig "4-8-4 242 A1" 60 6000 70000 1945;
    make V200BB Diesel "V200 B-B" 90 6000 100000 1955;
    make BoBoBo Diesel "Re 6/6 B-B-B" 70 7000 100000 1968;
    make TGV Diesel "TresGrandVitesse" 160 8000 150000 1978;
  ]

let of_region = function
  | Region.WestUS | EastUS -> us_engines
  | Britain -> en_engines
  | Europe -> eu_engines


let randomize_year rand engines =
  List.mapi (fun i engine ->
    match i with
    | 0 -> engine
    | 1 -> {engine with year = engine.year - Random.int 4 rand}
    | _ -> {engine with year = engine.year + 4 - Random.int 8 rand}
  )
  engines

let t_of_make engines make =
  List.find (fun engine -> equal_make engine.make make) engines
  
let available_at_year engines ~year =
  List.filter (fun engine -> engine.year <= year && year - engine.year <= 50) engines

