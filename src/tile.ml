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

