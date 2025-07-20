open Containers
module C = Constants

type t =
  | Clear (* 0 *)
  | Woods
  | Swamp
  | Foothills
  | Hills
  | Mountains (* 5 *)
  | City
  | Village
  | Farm
  | Slums
  | FoodProc (* 10 *)
  | Ranch (* US *)
  | Stockyard (* US *)
  | Factory
  | GrainElev (* US, Eng *)
  | PaperMill (* 15, US *)
  | Landing of Dir.Set.t (* Light blue river *)
  | LumberMill (* US *)
  | CoalMine
  | SteelMill
  | PowerPlant (* 20 US, Europe *)
  | OilWell (* US *)
  | Refinery (* US *)
  | EnemyStation of {over: t; owner: Owner.t} (* Tile it's covering *)
  | River of Dir.Set.t (* directions of river *)
  | Ocean of Dir.Set.t (* 25 *) (* dirs are directions of land *)
  | Harbor of Dir.Set.t (* same as ocean *)

  (* Alternative *)
  | Desert
  | SaltMine (* Eng *)
  | TextileMill (* Eng, Eur *)
  | ChemicalPlant (* Eng, Eur *)
  | Brewery (* Eng *)
  | Vineyard (* Eur *)
  | Winery (* Eur *)
  | Fort (* Eur *)
  | GlassWorks (* Eng *)
  | SheepFarm (* Eng, Eur *)
  [@@deriving eq, yojson, show]

let default_enemy_station = EnemyStation{owner=C.player; over=Clear}

let show = function
  | Clear -> "Clear"
  | Woods -> "Woods"
  | Swamp -> "Swamp"
  | Foothills -> "Foothills"
  | Hills -> "Hills"
  | Mountains -> "Mountains"
  | City -> "City"
  | Village -> "Village"
  | Farm -> "Farm"
  | Slums -> "Slums"
  | FoodProc -> "Food Proc."
  | Ranch -> "Ranch"
  | Stockyard -> "Stockyard"
  | Factory -> "Factory"
  | GrainElev -> "Grain Elev."
  | PaperMill -> "Paper Mill"
  | Landing _ -> "Landing"
  | LumberMill -> "Lumber Mill"
  | CoalMine -> "Coal Mine"
  | SteelMill -> "Steel Mill"
  | PowerPlant -> "Power Plant"
  | OilWell -> "Oil Well"
  | Refinery -> "Refinery"
  | EnemyStation _ -> "Enemy Station"
  | River _ -> "River"
  | Ocean _ -> "Ocean"
  | Harbor _ -> "Harbor"
  | Desert -> "Desert"
  | SaltMine -> "Salt Mine"
  | TextileMill -> "Textile Mill"
  | ChemicalPlant -> "Chemical Plant"
  | Brewery -> "Brewery"
  | Vineyard -> "Vineyard"
  | Winery -> "Winery"
  | Fort -> "Fort"
  | GlassWorks -> "Glass Works"
  | SheepFarm -> "Sheep Farm"

let to_enum = function
  | Clear -> 0
  | Woods -> 1
  | Swamp -> 2
  | Foothills -> 3
  | Hills -> 4
  | Mountains  -> 5
  | City -> 6
  | Village -> 7
  | Farm -> 7
  | Slums -> 8
  | FoodProc -> 9
  | Ranch  -> 10
  | Stockyard -> 11
  | Factory -> 12
  | GrainElev  -> 13
  | PaperMill  -> 14
  | Landing _ -> 15
  | LumberMill  -> 16
  | CoalMine -> 17
  | SteelMill -> 18
  | PowerPlant  -> 19
  | OilWell  -> 20
  | Refinery  -> 21
  | EnemyStation _ -> 22
  | River _ -> 23
  | Ocean _ -> 24
  | Harbor _ -> 25
  | Desert -> 26
  | SaltMine -> 27
  | TextileMill -> 28
  | ChemicalPlant -> 29
  | Brewery -> 30
  | Vineyard -> 31
  | Winery -> 32
  | Fort -> 33
  | GlassWorks -> 34
  | SheepFarm -> 35

let is_ground = function
  | River _ | Ocean _ | Harbor _ | Landing _ | EnemyStation _ -> false
  | _ -> true

let is_enemy_station = function
  | EnemyStation _ -> true
  | _ -> false

  (* Whether a tile is something you can build *)
let is_buildable = function
  | FoodProc
  | Ranch
  | Stockyard
  | Factory
  | GrainElev
  | PaperMill
  | LumberMill
  | CoalMine
  | SteelMill
  | PowerPlant
  | OilWell
  | Refinery
  | SaltMine
  | TextileMill
  | ChemicalPlant
  | Brewery
  | Vineyard
  | Winery
  | GlassWorks -> true
  | _ -> false

module Info = struct

  module TileHash = Hashtbl.Make(struct
    (* Specialized hashtable that ignores the inner arguments of the tile *)
    type nonrec t = t
    let equal x y =
      to_enum x = to_enum y
    let hash = to_enum
  end)

  type t = {
    cost: Money.t;
    supply: (Goods.t * int) list;
    demand: (Goods.t * int) list;
  } [@@deriving show]

  let make ?(supply=[]) ?(demand=[]) cost =
  let cost = Money.of_int cost in
  {
    cost;
    supply=supply;
    demand=demand;
  }

  (* each car is 32? *)
  (* demand: 64 for one full demand,
        so e.g. need 2 cities to demand one food
     supply: 32 for one full supply. mail/passengers are less.
   *)
  open Goods
  let empty = Dir.Set.empty
  let d = 64
  let s = 32
  let std_list = [
      Clear, make 1;
      Woods, make 0;
      Swamp, make 0;
      Desert, make 0;
      Foothills, make 0;
      Hills, make 0;
      Mountains, make 0;
      River empty, make 0;
      Ocean empty, make 10;
      Farm, make 3;
      Slums, make 4;
      default_enemy_station, make 0;
  ]

  let us_list = (std_list @ [
      City, make 10
        ~supply:[Mail, 24; Passengers, 32] (* 1, 2 *)
        ~demand:[Mail, d/2; Passengers, d/2; Food, d/2; Textiles, d/2];
      Village, make 5
        ~supply:[Mail, 4; Passengers, 12]  (* ?, 1 *)
        ~demand:[Mail, d/8; Passengers, d/4; MfgGoods, d/4];
      FoodProc, make 10 ~demand:[Fertilizer, d];
      Ranch, make 5 ~supply:[Livestock, 128];
      Stockyard, make 10 ~demand:[Livestock, d];
      Factory, make 20 ~demand:[Steel, d];
      GrainElev, make 10 ~supply:[Grain, 128];
      PaperMill, make 10 ~demand:[Wood, 64];
      Landing empty, make 0 ~demand:[Grain, 64; Coal, 64];
      LumberMill, make 5 ~supply:[Wood, 64];
      CoalMine, make 5 ~supply:[Coal, 64];
      SteelMill, make 15 ~demand:[Coal, 96];
      PowerPlant, make 25 ~demand:[Petroleum, 96; Wood, 96; Coal, 96];
      OilWell, make 10 ~supply:[Petroleum, 96];
      Refinery, make 15 ~demand:[Petroleum, 64];
      Harbor empty, make 20 ~supply:[MfgGoods, 128] ~demand:[Grain, 64; Coal, 64];
  ])

  let us_convert = [
      Livestock, Food;
      Fertilizer, Food;
      Steel, MfgGoods;
      Petroleum, MfgGoods;
      Cotton, Textiles;
      Coal, Steel;
  ] |> Hashtbl.of_list

  let eu_list = (std_list @ [
      City, make 10
        ~supply:[Mail, 24; Passengers, 32]
        ~demand:[Mail, 32; Passengers, 32; Wine, 32; Textiles, 32];
      Village, make 5
        ~supply:[Mail, 4; Passengers, 12]
        ~demand:[Mail, 8; Passengers, 16; Fertilizer, 16]; (* farms I guess *)
      Harbor empty, make 20 ~supply:[Wool, 192] ~demand:[Wine, 64; Armaments, 64];
      Factory, make 20 ~supply:[Armaments, 32] ~demand:[Steel, 64];
      CoalMine, make 5 ~supply:[Coal, 96];
      SteelMill, make 15 ~demand:[Coal, 64];
      (* TODO: power plant conversion makes no sense. Only due to simplicity of conversion system *)
      PowerPlant, make 25 ~demand:[Coal, 64; Nitrates, 64];
      SheepFarm, make 5 ~supply:[Nitrates, 96; Wool, 96]; (* Eng, Eur *)
      TextileMill, make 10 ~demand:[Wool, 64];(* Eng, Eur *)
      ChemicalPlant, make 15 ~demand:[Nitrates, 64]; (* Eng, Eur *)
      Vineyard, make 10 ~supply:[Grapes, 128]; (* Eur *)
      Winery, make 10 ~demand:[Grapes, 64]; (* Eur *)
      Fort, make 5 ~demand:[Armaments, 64]; (* Eur *)
    ])

  let en_list = (std_list @ [
      (* Note: this seems like a limitation. Livestock should not go to city except as food,
         but there's no space for the food proc plant in the economy *)
      City, make 10
        ~supply:[Mail, 24; Passengers, 32]
        ~demand:[Mail, 32; Passengers, 32; Livestock, 32; Textiles, 32];
      Village, make 5
        ~supply:[Mail, 4; Passengers, 12]
        ~demand:[Mail, 8; Passengers, 16; Beer, 16];
      Landing empty, make 0 ~demand:[MfgGoods, 64];
      Harbor empty, make 20 ~supply:[Hops, 128; Cotton, 256] ~demand:[MfgGoods, 64];
      Factory, make 20 ~demand:[Steel, 64];
      CoalMine, make 5 ~supply:[Coal, 3 * 32];
      SteelMill, make 15 ~demand:[Coal, 64];
      SaltMine, make 5 ~supply:[Chemicals, 3 * 32];  (* Eng *)
      TextileMill, make 10 ~demand:[Cotton, 64];(* Eng, Eur *)
      ChemicalPlant, make 15 ~demand:[Chemicals, 64]; (* Eng, Eur *)
      GrainElev, make 10 ~supply:[Hops, 64];
      Brewery, make 10 ~demand:[Hops, 64]; (* Eng *)
      GlassWorks, make 10 ~supply:[MfgGoods, 5 * 32]; (* Eng *)
      SheepFarm, make 5 ~supply:[Livestock, 5 * 32]; (* Eng, Eur *)
    ])

  let us_tbl = List.to_seq us_list |> TileHash.of_seq
  let eu_tbl = List.to_seq eu_list |> TileHash.of_seq
  let en_tbl = List.to_seq en_list |> TileHash.of_seq

  let get region tile =
    let tbl = match region with
      | Region.WestUS | EastUS -> us_tbl
      | Europe -> eu_tbl
      | Britain -> en_tbl
    in
    TileHash.find tbl tile

  let map_industry region f =
    let list = match region with
      | Region.WestUS | EastUS -> us_list
      | Europe -> eu_list
      | Britain -> en_list
    in
    list
    |> List.filter (fun (tile, _) -> is_buildable tile)
    |> List.map f

  let build_cost_of_tile region tile =
    let data = get region tile in
    Money.(data.cost * C.build_industry_mult)

  (* Choose the "best" supply/demand excluding mail *)
  let resource_map_supply_demand info =
    let supply =
      List.fold_left (fun min (good, x) ->
        if Goods.(good >= Goods.Passengers) && Int.(x >= 16) then
          match min with
          | Some min_good when good < min_good -> Some good
          | Some _ -> min
          | None -> Some good
        else min)
      None
      info.supply
    in
    let demand =
      List.fold_left (fun min (good, _) ->
        if Goods.(good >= Goods.Passengers) then
          match min with
          | Some min_good when good < min_good -> Some good
          | Some _ -> min
          | None -> Some good
        else min)
      None
      info.demand
    in
    match supply, demand with
    | Some supply, _ -> `Supply supply
    | _, Some demand -> `Demand demand
    | _ -> `None

end

let (=) = equal
