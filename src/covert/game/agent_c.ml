open! Containers

(* Agent stuff that works with Case *)

let get_or_gen (s:Services.t) org_id loc_id (case:Case.t) =
  Agent.S.get_or_gen s org_id loc_id ~mm_agent:(Case.G.mm case) (Case.G.agents case) (Case.G.orgs case)
