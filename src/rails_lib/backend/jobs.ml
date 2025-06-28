open! Containers

type eu =
  (* Europe *)
  [ `ChimneySweep
  | `GardenClubChairman
  | `DancingInstructor
  | `Butler
  | `Surveyor
  | `GameWarden
  | `StableMaster
  | `SafariLeader
  | `Harbormaster
  | `PolarExplorer
  | `ConsultingDetective
  | `InvestmentBanker
  | `ArmamentsMerchant
  | `Magistrate
  | `NavyCommodore
  | `ChiefOfSecretService
  | `ArmyInspectorGeneral
  | `MinisterOfFinance
  | `RoyalAdvisor
  | `PrimeMinister]
  [@@deriving enum, yojson, ord, eq]

type us =
  (* US *)
  [ `Hobo
  | `SnakeOilPeddler
  | `IndianAgent
  | `RiverboatGambler
  | `Conductor
  | `ArmyCaptain
  | `NewspaperEditor
  | `Inventor
  | `SteamboatCaptain
  | `Mayor
  | `CircusImpresario
  | `SteamshipOwner
  | `BankPresident
  | `Congressman
  | `PresidentOfTheRRTrust
  | `StateGovernor
  | `PresidentOfStockExchange
  | `SecretaryOfTreasury
  | `GeneralOfArmies
  | `PresidentOfUnitedStates]
  [@@deriving enum, yojson, ord, eq]

type t = [ eu | us ]
  [@@deriving yojson, eq]

let max = eu_to_enum `PrimeMinister

let min region = if Region.is_us region then `Hobo else `ChimneySweep

let of_enum region n =
  let v =
    if Region.is_us region then us_of_enum n
    else eu_of_enum n 
  in
  v |> Option.get_exn_or "Invalid enumeration"

let to_enum = function
  | `ChimneySweep -> 0
  | `GardenClubChairman -> 1
  | `DancingInstructor -> 2
  | `Butler -> 3
  | `Surveyor -> 4
  | `GameWarden -> 5
  | `StableMaster -> 6
  | `SafariLeader -> 7
  | `Harbormaster -> 8
  | `PolarExplorer -> 9
  | `ConsultingDetective -> 10
  | `InvestmentBanker -> 11
  | `ArmamentsMerchant -> 12
  | `Magistrate -> 13
  | `NavyCommodore -> 14
  | `ChiefOfSecretService -> 15
  | `ArmyInspectorGeneral -> 16
  | `MinisterOfFinance -> 17
  | `RoyalAdvisor -> 18
  | `PrimeMinister -> 19
  | `Hobo -> 0
  | `SnakeOilPeddler -> 1
  | `IndianAgent -> 2
  | `RiverboatGambler -> 3
  | `Conductor -> 4
  | `ArmyCaptain -> 5
  | `NewspaperEditor -> 6
  | `Inventor -> 7
  | `SteamboatCaptain -> 8
  | `Mayor -> 9
  | `CircusImpresario -> 10
  | `SteamshipOwner -> 11
  | `BankPresident -> 12
  | `Congressman -> 13
  | `PresidentOfTheRRTrust -> 14
  | `StateGovernor -> 15
  | `PresidentOfStockExchange -> 16
  | `SecretaryOfTreasury -> 17
  | `GeneralOfArmies -> 18
  | `PresidentOfUnitedStates -> 19

let show = function
  | `ChimneySweep -> "Chimney Sweep"
  | `GardenClubChairman -> "Garden-Club Chairman"
  | `DancingInstructor -> "Dancing Instructor"
  | `Butler -> "Butler"
  | `Surveyor -> "Surveyor"
  | `GameWarden -> "Game Warden"
  | `StableMaster -> "Stable Master"
  | `SafariLeader -> "Safari Leader"
  | `Harbormaster -> "Harbormaster"
  | `PolarExplorer -> "Polar Explorer"
  | `ConsultingDetective -> "Consulting Detective"
  | `InvestmentBanker -> "Invesment Banker"
  | `ArmamentsMerchant -> "Armaments Merchant"
  | `Magistrate -> "Magistrate"
  | `NavyCommodore -> "Navy Commodore"
  | `ChiefOfSecretService -> "Chief Of Secret Service"
  | `ArmyInspectorGeneral -> "Army Inspector General"
  | `MinisterOfFinance -> "Minister Of Finance"
  | `RoyalAdvisor -> "Royal Advisor"
  | `PrimeMinister -> "Prime Minister"
  | `Hobo -> "Hobo"
  | `SnakeOilPeddler -> "Snake Oil Peddler"
  | `IndianAgent -> "Indian Agent"
  | `RiverboatGambler -> "Riverboat Gambler"
  | `Conductor -> "Conductor"
  | `ArmyCaptain -> "Army Captain"
  | `NewspaperEditor -> "Newspaper Editor"
  | `Inventor -> "Inventor"
  | `SteamboatCaptain -> "Steamboat Captain"
  | `Mayor -> "Mayor"
  | `CircusImpresario -> "Circus Impresario"
  | `SteamshipOwner -> "Steamship Owner"
  | `BankPresident -> "Bank President"
  | `Congressman -> "Congressman"
  | `PresidentOfTheRRTrust -> "President Of The RR Trust"
  | `StateGovernor -> "State Governor"
  | `PresidentOfStockExchange -> "President Of The Stock Exchange"
  | `SecretaryOfTreasury -> "Secretary Of The Treasury"
  | `GeneralOfArmies -> "General Of The Armies"
  | `PresidentOfUnitedStates -> "President Of The United States"

let fold region f acc =
  let max_val = max in
  Iter.fold (fun acc i -> f acc (of_enum region i))
    acc
    Iter.(0 -- max_val)

