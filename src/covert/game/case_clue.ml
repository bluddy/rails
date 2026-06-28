open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers
module C = Constants

open Case

(* let clue_gen ?(in_org_id=Org.cia) in_loc_id clue_amt clue_type (v:t) = *)
(*   let clue_amt = clue_mat + 1 in *)
(*   Agent.Map.fold (fun agent_id agent chosen_agent -> *)
(*     let org_id = Agent.S.to_org agents agent_id in *)
(*     let loc_id = Agent.S.to_loc agents agent_id in *)
(*     let org_to_cia_dist = Org.connection orgs org_id in_org_id in *)
(*     let loc_to_agent_loc_dist = Loc.connection locs loc_id in_loc_id in *)
(*     let chosen_agent = if org_to_cia_dist = 0 && loc_to_agent_loc_dist = 0 then agent_id else chosen_agent in *)
(*     let clue_div_dist = (clue_amt / ((loc_to_agent_loc_dist + 2) * (org_to_cia_dist + 2))) / 256 in *)
(*     let clue_div_dist2 = (clue_amt / ((loc_to_agent_loc_dist + 6) * (org_to_cia_dist + 3))) / 64 in *)
(*     () *)
(*   ) *)
(*   agents *)
(*   None *)
(**)
