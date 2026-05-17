open! Containers

type t =
  | Recruit
  | Operative
  | Technician
  | Agent
  | Organizer
  | Special_agent
  | Group_leader
  | Mastermind
  [@@deriving yojson, enum, show]

let show = function
  | Recruit -> "Recruit"
  | Operative -> "Operative"
  | Technician -> "Technician"
  | Agent -> "Agent"
  | Organizer -> "Organizer"
  | Special_agent -> "Special Agent"
  | Group_leader -> "Group_leader"
  | Mastermind -> "Mastermind"
