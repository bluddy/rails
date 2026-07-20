open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers

type setup = {
  crime: Crime.Id.t;
  failed_steps: Crime.Step.Set.t;
  step: Crime.Step.t;
  region: Region.t;
  mm: Agent_d.t;
} [@@deriving yojson]

type data = {
  locs: Loc.map;
  orgs: Org.map;
  double_agents: Loc.Set.t;
  agents: Agent_d.map;
  roles: Role.map;
  hqs: Hq.map;
  events: Event_d.map;
  actions: Action.map;
  items: Item.map;
  clues: Clue_d.map;
} [@@deriving yojson]

type t = {
  s: setup;
  world: World.t;
  d: data;
  time: Time.t;
  cur_loc: Loc.Id.t;
  cur_org: Org.Id.t;
  enemy_anxiety: int;
  agent_autoescape: Agent_d.Id.t option; (* we won't roll for this agent *)
  agent_jailbreak: Agent_d.Id.t option;
} [@@deriving yojson]

module G = struct
  let step v = v.s.step
  let region v = v.s.region
  let crime v = v.s.crime
  let orgs v = v.d.orgs
  let locs v = v.d.locs
  let double_agents v = v.d.double_agents
  let roles v = v.d.roles
  let hqs v = v.d.hqs
  let events v = v.d.events
  let actions v = v.d.actions
  let agents v = v.d.agents
  let difficulty v = v.world.difficulty
  let items v = v.d.items
  let clues v = v.d.clues
end
module U = struct
  let agents agents v = {v with d={v.d with agents}}
  let events events v = {v with d={v.d with events}}
  let actions actions v = {v with d={v.d with actions}}
  let items items v = {v with d={v.d with items}}
  let orgs orgs v = {v with d={v.d with orgs}}
  let roles roles v = {v with d={v.d with roles}}
end

let update_events fn v = U.events (fn v.d.events) v
let crime_type v = Crime.Step.get_type v.s.crime v.s.step

