open! Containers


(* The game uses a funky init algo for some reason *)
let init_matrix = [
 [1; 1; 3], [2; 2; 2; 2];
 [0; 0; 1], [3; 3; 3; 3];
 [0; 1; 0], [5; 3; 4; 6];
 [0; 0; 3], [8; 4; 6; 1];
 [0; 1; 0], [5; 5; 5; 5];
 [0; 1; 3], [6; 6; 6; 6];
 [0; 1; 3], [4; 8; 5; 2];
 [0; 1; 1], [7; 7; 8; 7];
 [0; 1; 2], [1; 2; 3; 7];
 [0; 1; 3], [3; 1; 8; 5];
 [0; 1; 3], [2; 2; 2; 3];
 [0; 0; 2], [3; 3; 3; 1];
 [0; 1; 3], [4; 8; 5; 3];
 [0; 0; 3], [8; 4; 6; 5];
 [0; 1; 0], [7; 7; 8; 8];
 [0; 1; 3], [6; 6; 6; 8];
 [1; 0; 3], [2; 2; 2; 2];
 [1; 0; 1], [4; 4; 4; 4];
 [1; 0; 0], [7; 1; 8; 5];
 [1; 0; 3], [8; 5; 3; 7];
 [1; 1; 1], [1; 8; 4; 2];
 [1; 1; 0], [4; 7; 2; 1];
 [1; 1; 0], [5; 2; 6; 3];
 [1; 1; 3], [5; 5; 5; 5];
 [1; 0; 3], [5; 3; 1; 6];
 [1; 0; 0], [2; 6; 7; 8];
 ]

(* Produce the fixed name/look init code for each mastermind *)
let init_mm_code (l1, l2) =
  let x = List.nth l1 1 * 2 + List.nth l1 2 * 4 in
  let x = x - List.nth l1 0 + 1 in
  List.foldi (fun x i v ->
    let offset = i * 3 + 4 in
    let part = (v - 1) lsl offset in
    x + part)
  x
  l2

let mm_codes = List.map init_mm_code init_matrix |> Array.of_list

let agent_of_org org_id loc_id orgs =
  let org = Org.Map.find org_id orgs in
  let name_offset = Org.get_name_offset org in
  let global_id = Org.get_global_id org |> Option.get_exn_or "oops" |> Org.Global_id.to_int in
  let id_code = mm_codes.(global_id) in
  Agent.create ~name_offset id_code org_id loc_id


