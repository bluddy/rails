open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers

type t = {
  (* mostly constant in the case *)
  crime: Crime.Id.t;
  failed_steps: Crime.Step.Set.t;
  step: Crime.Step.t;
  region: Region.t;
  mm: Agent.t;
  world: World.t;

  (* dynamic in the case *)
  cur_loc: Loc.Id.t;
  cur_org: Org.Id.t;
  locs: Loc.map;
  orgs: Org.map;
  enemy_anxiety: int;
  double_agents: Loc.Set.t;
  roles: Role.map;
  agents: Agent.map;
  events: Event.map;
  hqs: Hq.map;
  actions: Action.map;
} [@@deriving yojson]

