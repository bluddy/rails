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
  events: Event.map;
  actions: Action.map;
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
let set_events events v = {v with d={v.d with events}}
let update_events fn v = set_events (fn v.d.events) v

