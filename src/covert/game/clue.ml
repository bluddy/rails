open! Containers

include Clue_d
module Sub = Subst_engine

let text_of_name_idx_ name_idx =
  let bottom = name_idx lsr 8 in
  let offset = ((name_idx lsr 4) land 0x7) lsl 1 + (bottom land 0xF) lsl 4 in
  let num = name_idx land 0x3F + 1 in
  let suffix = match bottom land 0xF with
    | 5 -> "00"
    | 6 -> "000"
    | _ -> ""
  in
  Printf.sprintf "%s%d%s" (Clue_d.names.(offset)) num suffix

let clue_text (s:Services.t) diff crime_type clue_idx orgs agents roles locs clues =
  let clue = Map.find clue_idx clues in
  let pat = Printf.sprintf "*C%d%d" ((Id.to_int clue_idx) mod 16) @@ Connect.to_enum clue.connect in
  let clue_name = text_of_name_idx_ clue.name_idx in
  let pats = [(Sub.Pattern.Victim, clue_name)] in
  let cur_agent_face = match clue.connect with
    | Face agent_id -> Some agent_id
    | _ -> None
  in
  let pats = match clue.connect with
    | Connect.Agent agent_id ->
        let name = Agent.S.name agent_id agents in
        (Sub.Pattern.SndOrg, name)::pats
    | Org org_id ->
        let org = Org.Map.find org_id orgs in
        (Sub.Pattern.RcvOrg, org.Org.name)::pats
    | Loc loc_id ->
        let loc = Loc.Map.find loc_id locs in
        (Sub.Pattern.SndLoc, loc.Loc.city)::pats
    | _ -> pats
  in
  let skip_chars, pat = match clue.connect with
    | Connect.Role role_id when Role.S.test_with_diff_div_4 diff role_id roles ->
        let id_code =
          let agent_id = Role.S.to_agent roles role_id in
          Agent.Map.find agent_id agents |> Agent.G.id_code
        in
        let pat = Printf.sprintf "*C0%d0%d" (id_code / 32) (id_code / 6) in
        2, pat
    | Role role_id ->
        let pat = Printf.sprintf "*C%02d%02d" crime_type (Role.Id.to_int role_id) in
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
  txt, clue_method, cur_agent_face

let create_name_idx_ (s:Services.t) role_id difficulty roles agents =
  let role = Role.Map.find role_id roles in
  let clue_random = (role.Role.clue_seed land 0xFF) / 2 in
  let num =
    if Role.S.test_with_diff_div_4 difficulty role_id roles then
      let agent_id = Role.S.to_agent roles role_id in
      let id_code = (Agent.Map.find agent_id agents).Agent.id_code in
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

let create org_id (s:Services.t) diff loc_id role_id known roles locs orgs agents _clues =
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
  let name_idx = create_name_idx_ s role_id diff roles agents in
  let clue = {
    org=org_id;
    loc=loc_id;
    role=role_id;
    connect;
    name_idx;
  }
  in
  clue, orgs, locs

