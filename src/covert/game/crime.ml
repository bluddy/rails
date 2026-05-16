open Containers
module C = Constants

type t = {
  title: string;
  org_bits: int; (* bitmask right now. there are 4 bit values *)
  step_types: int array; (* steps in crime *)
  objects: string array;
}

let make title org_bits step_types objects =
  let step_types = Array.of_list step_types in
  let objects = Array.of_list objects in
  {org_bits; step_types; objects; title}

let crimes = [|
  make "Welcome Max" 0xF [1]
    ["the US embassy";
    "a top secret code book"];
  make "Keyhole Satellite" 0x9 [0; 6; 4]
    ["satellite expert Xavier Rayban";
    "";
    "the uplink ground station";
    "";
    "CIA storage";
    "X-Band radio transmitter"]
    ;
  make "Freedom Games" 0xC [7; 2; 6]
    ["the Freedom Games security plan";
    "";
    "an Israeli security expert";
    "";
    "the Israeli athletic dormitory";
    ""];
  make "SuperDrug" 0x6 [0; 4; 3]
    ["the noted chemist Dr Pasture";
    "";
    "a large industrial warehouse";
    "chemicals";
    "a new super drug";
    ""];
  make "Liberian Election" 9 [0xB; 6; 8]
    ["Geraldo Corazon";
    "";
    "the Election Commission HQ";
    "";
    "the Social Democrat campaign rally";
    ""];
  make "Summit Conference" 0xC [0xA; 9; 2]
    ["Carlos the Hyena";
    "";
    "summit ID documents";
    "";
    "a leading Head of State";
    ""];
  make "Rampant Inflation" 0xA [0xA; 4; 0xC]
    ["Willie the Pen";
    "";
    "the Treasury Building";
    "Currency Plates";
    "medium denomination currency";
    "currency paper"];
  make "Stolen Nuke" 0xC [7; 9; 4]
    ["Airbase Passwords";
    "";
    "Airbase ID Cards";
    "";
    "an Airbase";
    "a Nuclear Bomb"];
  make "Super Cannon" 0xB [4; 4; 0xC]
    ["a chemical plant";
    "explosive propellants";
    "a metal foundry";
    "high-strength tubes";
    "the super cannon";
    "precision lathe"];
  make "Build Nuclear Weapon" 0xD [4; 0; 0xC]
    ["a breeder reactor";
    "plutonim 235";
    "physicist Dr A. Einstone";
    "";
    "a nuclear bomb";
    "imploding detonator"];
  make "Terror Campaign" 0xC [6; 8; 2]
    ["train station";
    "";
    "the Veteran's Day parade";
    "";
    "a Supreme Court Justice";
    ""];
  make "Virus X" 0xC [0; 4; 0xC]
    ["bacteriologist Edgar Coli";
    "";
    "the medical college";
    "virulent virus samples";
    "deadly virus X";
    "one million petri dishes"];
  make "Drug Shortage" 0xA [6; 2; 3]
    ["Interpol Headquarters";
    "";
    "chief drug investigator";
    "";
    ""];
  make "Train Wreck" 0xC [7; 5; 6]
    ["VIP travel itinerary";
    "";
    "the locomotive plant";
    "train schematics";
    "the train station";
    ""];
  make "Prison Break" 7 [9; 0xB; 0xA]
    ["prison visitor pass";
    "";
    "the prison warden";
    "";
    "Manny Noriega";
    ""];
  make "Stealth Fighter" 3 [0xB; 7; 4]
    ["fighter pilot";
    "";
    "airbase operation plans";
    "";
    "the secret hangar";
    "stealth fighter";
    ]
|]

module Id = Engine.Int_id.Make()

let none = Id.of_int (-1)

let tutorial = Id.of_int 0

let random r = Random.int_range 1 (Array.length crimes - 1) r
  |> Id.of_int

let check_org_support crime_id org_id orgs =
  let org = Org.Map.find org_id orgs in
  let bits = Org.get_bits org in
  let crime = crimes.(Id.to_int crime_id) in
  bits land crime.org_bits > 0

module Step = struct
  (* Crime steps in a crime *)
  module G_Id = Id
  module Id = Engine.Int_id.Make()
  module Set = struct
      include Utils.Set.Make(struct
      type t = Id.t [@@deriving yojson]
      let compare = Id.compare
    end)

    let random r v =
      let l = to_list v in
      Random.choose_return l r

  end

  let none = Id.of_int (-1)

  include Id

  let get_all crime =
    let crime = G_Id.to_int crime in
    let crime = crimes.(crime) in
    crime.step_types |> Array.to_list
    |> List.mapi (fun i _ -> Id.of_int i)

  let is_last crime step =
    let crime = crimes.(G_Id.to_int crime) in
    let len = Array.length crime.step_types in
    Id.to_int step = len - 1

  let is_valid crime step =
    let crime, step_num = crimes.(G_Id.to_int crime), Id.to_int step in
    let len = Array.length crime.step_types in
    step_num < len

  let get_type crime step =
    let crime, step = G_Id.to_int crime, Id.to_int step in
    crimes.(crime).step_types.(step)

  let get_objs crime step =
    let crime, step = G_Id.to_int crime, Id.to_int step in
    let idx1 = step * 2 in
    let idx2 = idx1 + 1 in
    let objs = crimes.(crime).objects in
    objs.(idx1), objs.(idx2)

end

