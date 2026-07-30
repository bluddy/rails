open! Containers

let propagate_known action_id (case:Case_d.t) =
  (* From action_print_and_propagate_known *)
  let action = Action.Map.find action_id @@ Case_d.G.actions case in
  let agent1 = Action.G.agent1 action in
  ()
  
  





