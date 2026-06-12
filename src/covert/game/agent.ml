open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers

module Names = Agent_names

include Agent_d

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
let create ?(known=Known_data.Set.empty) id_code org_id loc_id orgs =
  let gender, name, last_name = gender_name_of_code org_id id_code orgs in
  {
    gender;
    org=org_id;
    name;
    last_name;
    id_code;
    loc=loc_id;
    known;
    roles=Role_d.Set.empty;
    roles_known=Role_d.Set.empty;
    status=At_large {anxiety=0};
  }

let mastermind = Id.of_int 0

let add_role role_id v =
  {v with roles=Role_d.Set.add role_id v.roles}

let add_role_known role_id v =
  {v with roles_known=Role_d.Set.add role_id v.roles_known}

let add_known_data known v =
  {v with known=Known_data.Set.add known v.known}

(* Should be rarely used *)
let remove_known_data known v =
  {v with known=Known_data.Set.remove known v.known}

let is_known known v = Known_data.Set.mem known v.known

let check_known l v = Known_data.Set.mem_any l v.known

let is_known_any v = Known_data.Set.not_empty v.known

let reduce_anxiety factor v = match v.status with
  | At_large {anxiety} ->
      let anxiety = anxiety - anxiety/factor in
      {v with status=At_large{anxiety}}
  | _ -> v

let is_double_agent v = match v.status with Double_agent -> true | _ -> false

let is_at_large v = match v.status with At_large _ -> true | _ -> false

let is_arrested v = match v.status with Arrested -> true | _ -> false

let go_into_hiding v = {v with status=In_hiding}

let go_free v = {v with status=At_large{anxiety=0}}

module S = struct

  let update_agent agent_id agents fn =
    Map.update agent_id (Option.map fn) agents

  let add_role agent_id role_id agents =
    update_agent agent_id agents @@ add_role role_id

  let add_role_known agent_id role_id agents =
    update_agent agent_id agents @@ add_role_known role_id

  let add_known_data agent_id known agents =
    update_agent agent_id agents @@ add_known_data known

  let remove_known_data agent_id known agents =
    update_agent agent_id agents @@ remove_known_data known

  let go_into_hiding agent_id agents =
    update_agent agent_id agents @@ go_into_hiding

  let of_role role_id roles agents =
    try
      let role = Role_d.Map.find role_id roles in
      let agent_id = role.Role_d.agent in
      let agent = Map.find agent_id agents in
      (agent_id, agent)
    with
    Not_found -> failwith @@ Printf.sprintf "Couldn't find agent of role %s" (Role.Id.show role_id)

  let to_loc agents agent_id = (Map.find agent_id agents).loc

  let get org_id loc_id agents =
    Map.find_pred (fun _ agent ->
      Org.Id.(agent.org = org_id) && Loc.Id.(agent.loc = loc_id))
      agents

  let get_or_gen (s:Services.t) org_id loc_id ~mm_agent agents orgs =
    match get org_id loc_id agents with
    | Some agent -> agent, agents
    | None ->
        let is_mm =
          Loc.Id.(mm_agent.loc = loc_id) && Org.Id.(mm_agent.org = org_id)
        in
        let agent_id =
          if is_mm then mastermind
          else
            (* Increase by 1 because 0 is mastermind *)
            Map.cardinal agents + 1 |> Id.of_int
        in
        let known = if is_mm then mm_agent.known else Known_data.Set.empty in
        let id_code =
          if is_mm then mm_agent.id_code
          else
            let id_code = Random.int 32766 s.random in
            let male = Random.int 3 s.random > 0 in
            if male then id_code lor 1 else id_code
        in
        let agent = create id_code ~known org_id loc_id orgs in
        let agents = Map.add agent_id agent agents in
        Printf.printf "New agent %s: %s\n" (Id.show agent_id) (yojson_of_t agent |> Yojson.Safe.to_string);
        agent_id, agents

  let reduce_anxiety factor v = Map.map (reduce_anxiety factor) v

end
