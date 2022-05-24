open Containers

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
  | EnemyRR
  | River of Dir.Set.t (* directions of river *)
  | Ocean of Dir.Set.t (* 25 *) (* dirs are directions of land *)
  | Harbor of Dir.Set.t (* same as ocean *)

  (* Alternative *)
  | Desert
  | SaltMine (* Eng *)
  | TextileMill (* Eng, Eur *)
  | ChemicalPlant (* Eng, Eur *)
  | Brewery (* Eng *)
  | Vinyard (* Eur *)
  | Winery (* Eur *)
  | Fort (* Eur *)
  | GlassWorks (* Eng *)
  | SheepFarm (* Eng, Eur *)
  [@@deriving eq, show]

let is_ground = function
  | River _ | Ocean _ | Harbor _ | Landing _ -> false
  | _ -> true

module Info = struct
  type nonrec t = {
    name: string;
    cost: int;
    supply: (Goods.t * int) list;
    demand: (Goods.t * int) list;
  }

  let make ?(supply=[]) ?(demand=[]) name cost = {
    name;
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
  let std_tbl = [
      Clear, make "Clear" 1;
      Woods, make "Woods" 0;
      Swamp, make "Swamp" 0;
      Desert, make "Desert" 0;
      Foothills, make "Foothills" 0;
      Hills, make "Hills" 0;
      Mountains, make "Mountains" 0;
      River empty, make "River" 0;
      Ocean empty, make "Ocean" 10;
      Farm, make "Farm" 3;
      Slums, make "Slums" 4;
  ]
  let us_tbl = [
      City, make "City" 10
        ~supply:[Mail, 24; Passengers, 32] (* 1, 2 *)
        ~demand:[Mail, d/2; Passengers, d/2; Food, d/2; Textiles, d/2];
      Village, make "Village" 5
        ~supply:[Mail, 4; Passengers, 12]  (* ?, 1 *)
        ~demand:[Mail, d/8; Passengers, d/4; MfgGoods, d/4];
      FoodProc, make "Food Proc." 10 ~demand:[Fertilizer, d];
      Ranch, make "Ranch" 5 ~supply:[Livestock, 128];
      Stockyard, make "Stockyard" 10 ~demand:[Livestock, d];
      Factory, make "Factory" 20 ~demand:[Steel, d];
      GrainElev, make "Grain Elev." 10 ~supply:[Grain, 128];
      PaperMill, make "Paper Mill" 10 ~demand:[Wood, 64];
      Landing empty, make "Landing" 0 ~demand:[Grain, 64; Coal, 64];
      LumberMill, make "LumberMill" 5 ~supply:[Wood, 64];
      CoalMine, make "Coal Mine" 5 ~supply:[Coal, 64];
      SteelMill, make "Steel Mill" 15 ~demand:[Coal, 96];
      PowerPlant, make "Power Plant" 25 ~demand:[Petroleum, 96; Wood, 96; Coal, 96];
      OilWell, make "Oil Well" 10 ~supply:[Petroleum, 96];
      Refinery, make "Refinery" 15 ~demand:[Petroleum, 64];
      Harbor empty, make "Harbor" 20 ~supply:[MfgGoods, 128] ~demand:[Grain, 64; Coal, 64];
  ]
  let us_convert = [
      Livestock, Food;
      Fertilizer, Food;
      Steel, MfgGoods;
      Petroleum, MfgGoods;
      Cotton, Textiles;
      Coal, Steel;
    ]
  let eu_tbl =
  [
      City, make "City" 10
        ~supply:[Mail, 24; Passengers, 32]
        ~demand:[Mail, 32; Passengers, 32; Wine, 32; Textiles, 32];
      Village, make "Village" 5
        ~supply:[Mail, 4; Passengers, 12]
        ~demand:[Mail, 8; Passengers, 16; Fertilizer, 16]; (* farms I guess *)
      Harbor empty, make "Harbor" 20 ~supply:[Wool, 192] ~demand:[Wine, 64; Armaments, 64];
      Factory, make "Factory" 20 ~supply:[Armaments, 32] ~demand:[Steel, 64];
      CoalMine, make "Coal Mine" 5 ~supply:[Coal, 96];
      SteelMill, make "Steel Mill" 15 ~demand:[Coal, 64];
      (* TODO: power plant conversion makes no sense. Only due to simplicity of conversion system *)
      PowerPlant, make "Power Plant" 25 ~demand:[Coal, 64; Nitrates, 64];
      SheepFarm, make "Sheep Farm" 5 ~supply:[Nitrates, 96; Wool, 96]; (* Eng, Eur *)
      TextileMill, make "Textile Mill" 10 ~demand:[Wool, 64];(* Eng, Eur *)
      ChemicalPlant, make "Chemical Plant" 15 ~demand:[Nitrates, 64]; (* Eng, Eur *)
      Vinyard, make "Vinyard" 10 ~supply:[Grapes, 128]; (* Eur *)
      Winery, make "Winery" 15 ~demand:[Grapes, 64]; (* Eur *)
      Fort, make "Fort" 20 ~demand:[Armaments, 64]; (* Eur *)
    ]
  let eu_convert =
    [
      Grapes, Wine;
      Steel, Armaments;
      Wool, Textiles;
      Nitrates, Fertilizer;
      Coal, Steel;
    ]
  let en_tbl =
  [
      (* Note: this seems like a limitation. Livestock should not go to city except as food,
         but there's no space for the food proc plant in the economy *)
      City, make "City" 10
        ~supply:[Mail, 24; Passengers, 32]
        ~demand:[Mail, 32; Passengers, 32; Livestock, 32; Textiles, 32];
      Village, make "Village" 5
        ~supply:[Mail, 4; Passengers, 12]
        ~demand:[Mail, 8; Passengers, 16; Beer, 16];
      Landing empty, make "Landing" 0 ~demand:[MfgGoods, 64];
      Harbor empty, make "Harbor" 20 ~supply:[Hops, 128; Cotton, 256] ~demand:[MfgGoods, 64];
      Factory, make "Factory" 20 ~demand:[Steel, 64];
      CoalMine, make "Coal Mine" 5 ~supply:[Coal, 3 * 32];
      SteelMill, make "Steel Mill" 15 ~demand:[Coal, 64];
      SaltMine, make "Salt Mine" 5 ~supply:[Chemicals, 3 * 32];  (* Eng *)
      TextileMill, make "Textile Mill" 10 ~demand:[Cotton, 64];(* Eng, Eur *)
      ChemicalPlant, make "Chemical Plant" 15 ~demand:[Chemicals, 64]; (* Eng, Eur *)
      Brewery, make "Brewery" 10 ~demand:[Hops, 64]; (* Eng *)
      GlassWorks, make "Glassworks" 15 ~supply:[MfgGoods, 5 * 32]; (* Eng *)
      SheepFarm, make "Sheep Farm" 5 ~supply:[Livestock, 5 * 32]; (* Eng, Eur *)
    ]
  let en_convert =
    [
      Hops, Beer;
      Steel, MfgGoods;
      Chemicals, MfgGoods;
      Cotton, Textiles;
      Coal, Steel;
    ]

end
