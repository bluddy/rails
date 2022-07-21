open Containers

type make =
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

type t = {
  make: make;
  name: string;
  max_speed: int;
  horsepower: int;
  price: int;
  year: int;
}

let make make name max_speed horsepower price year =
  { make; name; max_speed; horsepower; price; year }

let us_engines = 
  (* US *)
  [
    make Grasshopper "Grasshopper" 20 500 10000 1820;
    make Norris "4-2-0 Norris" 30 1000 20000 1833;
    make American "4-4-0 American" 40 1500 30000 1848;
    make Mogul "2-6-0 Mogul" 25 2000 30000 1851;
    make TenWheeler "4-6-0 Ten Wheeler" 45 2000 40000 1868;
    make Consolidation "2-8-0 Consolidation" 40 2500 40000 1877;
    make Pacific "4-6-2 Pacific" 60 3500 60000 1892;
    make Mikado "2-8-2 Mikado" 45 3500 50000 1903;
    make Mallet "2-6-6-2 Mallet" 50 4500 70000 1911;
    make FSeriesDiesel "'F' Series Diesel" 70 3500 75000 1916;
    make GPSeriesDiesel "'GP' Series Diesel" 60 4000 75000 1930;
  ]

let en_engines =
  (* England *)
  [
    make Planet "2-2-0 Planet" 20 500 10000 1820;
    make Patentee "2-2-2 Patentee" 30 1000 20000 1835;
    make IronDuke "4-2-2 Iron Duke" 40 1500 30000 1845;
    make DxGoods "0-6-0 DX Goods" 25 2500 30000 1855;
    make Stirling "4-2-2 Stirling" 45 2000 40000 1870;
    make MidlandSpinner "4-2-2 Spinner" 50 2500 50000 1880;
    make WebbCompound "0-8-0 Compound" 40 3000 50000 1890;
    make ClaudHamilton "4-4-0 Hamilton" 60 2500 60000 1900;
    make A1Class "4-6-2 Gresley" 50 4000 60000 1920;
    make A4Class "4-6-2 Class A4" 70 3000 70000 1930; (* EN: 3000 HP? *)
  ]

let eu_engines =
  (* Europe *)
  [
    make WebbCompound "0-8-0 Compound" 40 3000 50000 1880;
    make ClaudHamilton "4-4-0 Hamilton" 60 2500 60000 1890;
    make A1Class "4-6-2 Gresley" 45 4000 60000 1905;
    make A4Class "4-6-2 Class A4" 70 3500 70000 1915; (* EN: 3000 HP? *)
    make ClassCrocodile "6/6 Crocodile" 40 5000 50000 1925;
    make ClassE18 "Class E18 1-D-1" 80 5000 80000 1935;
    make R242A1 "4-8-4 242 A1" 60 6000 70000 1945;
    make V200BB "V200 B-B" 90 6000 100000 1955;
    make BoBoBo "Re 6/6 B-B-B" 70 7000 100000 1968;
    make TGV "TresGrandVitesse" 160 8000 150000 1978;
  ]

let get = function
  | Region.WestUS | EastUS -> us_engines
  | Britain -> en_engines
  | Europe -> eu_engines

