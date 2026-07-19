open! Containers

include Clue_d
module Sub = Subst_engine

let is_connect_role v = match v.connect with | Connect.Role _ -> true | _ -> false

(* For a non-role, we can get a short summary *)
let get_summary_text_non_role clue_id (case:Case_d.t) = (* Obtain a shorter text summary *)
  let clue = Map.find clue_id @@ Case_d.G.clues case in
  let connect_text () =
    let case = match clue.connect with
      | Org org_id ->
          let orgs = Org.S.add_known org_id @@ Case.G.orgs case in
          Case.U.orgs orgs case
      | _ -> case
    in
    let face = match clue.connect with
      | Connect.Face agent_id -> Some agent_id
      | _ -> None
    in
    let txt = match clue.connect with
    | Connect.Face _ -> "this face."
    | Agent agent_id ->
        let agent = Agent.Map.find agent_id @@ Case.G.agents case in
        agent.name^" "^agent.last_name
    | Org org_id ->
        let org = Org.Map.find org_id @@ Case.G.orgs case in
        org.name
    | Loc loc_id ->
        let loc = Loc.Map.find loc_id @@ Case.G.locs case in
        loc.city
    | Role _ -> failwith "Role not expected here"
    in
    txt, face, case
  in
  let clue_name = Name.get_text clue.id in
  let connect_words = Connect_word.get_text clue.id clue.connect in
  let con_text, face, case = connect_text () in
  let txt = Printf.sprintf "%s %s %s" clue_name connect_words con_text in
  txt, face, case

let get_text (s:Services.t) clue_id (case:Case_d.t) =
  let clue = Map.find clue_id @@ Case_d.G.clues case in
  let pat = Printf.sprintf "*C%d%d" ((Id.to_int clue_id) mod 16) @@ Connect.to_enum clue.connect in
  let clue_name = Name.get_text clue.id in
  let pats = [(Sub.Pattern.Victim, clue_name)] in
  let agents = Case_d.G.agents case in
  let pats = match clue.connect with
    | Connect.Agent agent_id ->
        let name = Agent.S.name agent_id agents in
        (Sub.Pattern.SndOrg, name)::pats
    | Org org_id ->
        let org = Org.Map.find org_id @@ Case_d.G.orgs case in
        (Sub.Pattern.RcvOrg, org.Org.name)::pats
    | Loc loc_id ->
        let loc = Loc.Map.find loc_id @@ Case_d.G.locs case in
        (Sub.Pattern.SndLoc, loc.Loc.city)::pats
    | _ -> pats
  in
  let roles, diff = Case_d.G.roles case, Case.G.difficulty case in
  let skip_chars, pat = match clue.connect with
    | Connect.Role role_id when Role.S.test_with_diff_div_4 diff role_id roles ->
        let id_code =
          let agent_id = Role.S.to_agent roles role_id in
          Agent.Map.find agent_id agents |> Agent.G.id_code
        in
        let pat = Printf.sprintf "*C0%d0%d" (id_code / 32) (id_code / 6) in
        2, pat
    | Role role_id ->
        let pat = Printf.sprintf "*C%02d%02d" (Case_d.crime_type case |> Crime.Type.to_int) (Role.Id.to_int role_id) in
        2, pat
    | _ -> 1, pat
  in
  let txt =
    let clue_txt = Hashtbl.find s.resources.text `Clues in
    Sub.get_lines ~pat clue_txt |> Option.get_exn_or "Clue text not found"
    |> Sub.subst_pat pats
  in
  let clue_method = (Char.to_int txt.[0]) land 0xF |> Method.of_enum |> Option.get_exn_or "oops" in
  let txt = String.drop skip_chars txt in
  txt, clue_method

let create_id (s:Services.t) role_id difficulty roles agents =
  let role = Role.Map.find role_id roles in
  let clue_random = (role.Role.clue_seed land 0xFF) / 2 in
  let num =
    if Role.S.test_with_diff_div_4 difficulty role_id roles then
      let agent_id = Role.S.to_agent roles role_id in
      let id_code = Agent.Map.find agent_id agents |> Agent.G.id_code in
      let v1 = (id_code lsr 3) mod 4 in
      let v2 = id_code mod 6 in
      let pat = Printf.sprintf "*C0%d0%d" v1 v2 in
      let clue_txt = Hashtbl.find s.resources.text `Clues in
      let txt = Sub.get_lines ~pat clue_txt |> Option.get_exn_or "Clue text not found" in
      let pats = Sub.Pattern.([Key, ""; Name, ""; Org, ""; City, ""; Role, ""]) in
      let txt = Sub.subst_pat pats txt in
      let c = ((txt.[1] |> Char.to_int) land 0xF) lsl 8 in
      c
    else
      role.clue_rand lsl 8
  in
  num + clue_random

let create org_id (s:Services.t) loc_id role_id source known case =
  let roles, agents, orgs, locs, clues, diff =
    let open Case_d in
    G.roles case, G.agents case, G.orgs case, G.locs case, G.clues case, G.difficulty case in
  let agent_id = Role.S.to_agent roles role_id in
  let connect = match known with
  | `Known_face -> Connect.Face agent_id
  | `Known_agent -> Agent agent_id
  | `Known_org -> Org (Agent.S.to_org agents agent_id)
  | `Known_role -> Role role_id
  | `Known_loc -> Loc (Agent.S.to_loc agents agent_id)
  in
  let orgs = match known with
  | `Known_org ->
      let org_id = Agent.S.to_org agents agent_id in
      orgs
      |> Org.S.incr_activity org_id
      |> Org.S.add_known org_id
  | _ -> orgs
  in
  let locs = match known with
  | `Known_loc -> Loc.S.incr_activity (Agent.S.to_loc agents agent_id) locs
  | _ -> locs
  in
  let roles = Role.S.add_known (known :> Known_data.t) role_id roles in
  let id = create_id s role_id diff roles agents in
  let clue = {
    org=org_id;
    loc=loc_id;
    role=role_id;
    connect;
    id;
    src=source;
  }
  in
  let clue_id = Map.num clues |> Id.of_int in
  let clues = Map.add clue_id clue clues in
  clue_id, {case with d={case.d with clues; locs; orgs}}

let known_to_discover random role_id roles (case:Case_d.t) =
  let role = Role.Map.find role_id roles in
  if Known_data.Set.all_standard role.Role.known then None else
  if Difficulty.lowest (Case_d.G.difficulty case) &&
    not @@ Role.check_known [`Known_role] role then Some `Known_role else
  if role.clue_rand = 2 (* From OG *)
    && Difficulty.(Case_d.G.difficulty case < Regional_conflict)
    && not @@ Role.check_known [`Known_loc] role then Some `Known_loc else
  let known = Utils.do_while
    (fun () -> Known_data.random random)
    (fun known -> Role.check_known [known] role)
  in
  Some known

let generate ?(in_org_id=Org.cia) in_loc_id clue_amt clue_type (case:Case_d.t) =
  let open Case_d in
  let agents, orgs, locs, roles = G.agents case, G.orgs case, G.locs case, G.roles case in
  let clue_amt = clue_amt + 1 in
  Agent.Map.fold (fun agent_id agent (chosen_agent, agents, roles) ->
    let org_id = Agent.S.to_org agents agent_id in
    let loc_id = Agent.S.to_loc agents agent_id in
    let org_to_cia_dist = Org.connection orgs org_id in_org_id in
    let loc_to_agent_loc_dist = Loc.connection locs loc_id in_loc_id in
    let chosen_agent = if org_to_cia_dist = 0 && loc_to_agent_loc_dist = 0 then Some agent_id else chosen_agent in
    let clue_div_dist = (clue_amt / ((loc_to_agent_loc_dist + 2) * (org_to_cia_dist + 2))) / 256 in
    let clue_div_dist2 = (clue_amt / ((loc_to_agent_loc_dist + 6) * (org_to_cia_dist + 3))) / 64 in
    let diff_factor = 10000 / ((G.difficulty case |> Difficulty.to_enum) + 2) / clue_div_dist  in
    let diff_factor_2 = diff_factor * diff_factor in
    let discover_val = ((Agent.G.discover_val agent / 2) + diff_factor + 1) / diff_factor_2 in
    let agents = Agent.S.update agent_id (Agent.U.discover_val discover_val) agents in
    let roles =
      Role.Set.fold (fun role_id (roles as acc) ->
        let role = Role.Map.find role_id roles in
        match Role.G.ctr_tick role with
        | Some tick when tick <= case.time.tick ->
            let w = 5000/((G.difficulty case |> Difficulty.to_enum) + 3) in
            let role = Role.U.ctr_discovery_add (w / clue_div_dist2) role in
            let rec loop role =
              let needed_val = Known_data.Set.to_discover_val (Role.G.known role) in
              let needed_val = (needed_val + 2) * (needed_val + 2) * 32 in
              let disc = Role.G.ctr_discovery role * ((Role.G.discover role) + 2) in
              if disc <= needed_val then role else
              if Known_data.Set.all_standard role.known then role else
              (* TODO: reveal clue
                 if clue_org is cia, clue method, else wiretap/photo
               *)
              loop role
            in
            let role = loop role in
            Role.Map.add role_id role roles
        | _ -> acc)
      agent.roles
      roles
    in
    chosen_agent, agents, roles)
  agents
  (None, agents, roles)

