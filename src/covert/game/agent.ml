open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers

module Names = Agent_names

type status =
  | Arrested
  | In_hiding
  | Double_agent
  | Escaped
  | Exchanged
  | Out_of_hiding
  | At_large
  | In_custody
  [@@deriving yojson]

type t = {
  gender: Gender.t;
  org: Org.Id.t;
  name: string;
  last_name: string;
  id_code: int; (* Used to generate the name, sex and picture *)
  loc: Loc.Id.t;
  known: Known_data.Set.t;
  role: Role.Set.t;
  role_known: Role.Set.t;
  status: status;
  anxiety: int;
} [@@deriving yojson]

let gender_name_of_code org_id x orgs =
  let gender = x land 1 in
  let name_idx = (x lsr 1) land 0xF in
  let last_name_idx = (x lsr 5) land 0xF in
  let name_arr = if gender = 0 then Names.female_names else Names.male_names in
  let org = Org.Map.find org_id orgs in
  let name_offset = Org.get_name_offset org in
  let name, last_name =
      name_arr.(name_offset + name_idx),
      Names.last_names.(name_offset + last_name_idx)
  in
  let gender = if gender = 0 then `Female else `Male in
  gender, name, last_name

  (* name_offset comes from org *)
let create id_code org_id loc_id orgs =
  let gender, name, last_name = gender_name_of_code org_id id_code orgs in
  {
    gender;
    org=org_id;
    name;
    last_name;
    id_code;
    loc=loc_id;
    known=Known_data.Set.empty;
    role=Role.Set.empty;
    role_known=Role.Set.empty;
    status=At_large;
    anxiety=0;
  }

module Id = Agent_id

module Map = Utils.Map.Make(struct
  type t = Id.t [@@deriving yojson]
  let compare = Id.compare
end)

type map = t Map.t [@@deriving yojson]

let mastermind = Id.of_int 0

let agent_get org_id loc_id agents =
  Map.find_pred (fun _ agent ->
    Org.Id.(agent.org = org_id) && Loc.Id.(agent.loc = loc_id))
    agents

    (*
let agent_get_or_gen (s:Services.t) org_id loc_id mm_agent agents =
  match agent_get org_id loc_id agents with
  | Some agent -> agent, agents
  | None ->
      let is_mm =
        Loc.Id.(mm_agent.loc = loc_id) && Org.Id.(mm_agent.org = org_id)
      in
      let agent_id =
        if is_mm then mastermind
        else Map.cardinal agents |> Id.of_int
      in
      let known = if is_mm then mm_agent.known else Known_data.Set.empty in
      let id_code =
        if is_mm then mm_agent.id_code
        else
          let id_code = Random.int 32766 s.random in
          let is_man = Random.int 3 s.random > 0 in
          if is_man then id_code lor 1 else id_code
      in
      *)









