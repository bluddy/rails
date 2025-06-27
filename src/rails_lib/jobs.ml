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
  [@@deriving enum]

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
  [@@deriving enum]

let max = eu_to_enum `PrimeMinister

type t = [ eu | us ]

