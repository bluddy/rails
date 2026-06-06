open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module String = Engine.String
module Gen = Engine.My_gen

include Event_d

let from_stream ~num_events s =
  Iter.fold (fun acc _ ->
    let role = Gen.get_bytei s |> Role.Id.of_int in
    let _junk = Gen.get_bytei s in
    let _tick_or_status = Gen.get_wordi s in
    let num_id = Gen.get_wordi s in
    let text = Gen.take 32 s |> Gen.to_stringi |> String.remove_nulls in
    let bits = Gen.get_wordi s in
    let item_bits = Gen.get_wordi s in
    let efficiency = Gen.get_wordi s in
    let use_anxiety = bits land 0x1000 > 0 in
    let kind =
      if bits land 0x2000 > 0 then Misc else
      let role = bits land 0xFF |> Role.Id.of_int in
      let inter = if bits land 0x800 > 0 then Meet else (assert (bits land 0x200 > 0); Msg) in
      let send_rcv = if bits land 0x100 > 0 then Rcv else Send in
      With_role {inter; send_rcv; role}
    in
    let event = {
      role;
      status=Ready;
      num_id;
      text;
      bits;
      item_bits;
      efficiency;
      kind;
      use_anxiety;
    } in
    print_endline @@ (yojson_of_t event |> Yojson.Safe.to_string);
    event::acc
  )
  []
  Iter.(0 -- (num_events - 1)) |> List.rev

let is_ready v = match v.status with Ready -> true | _ -> false

module S = struct

  (* Emulate this test in code *)

  let check_process_event event_id roles agents v =
    let event_has_prev_ready_same_role_ event_id v =
      let event = Map.find event_id v in
      let rec loop n =
        if Id.(event_id = n) then false else
        let event2 = Map.find n v in
        if is_ready event2 && Role.Id.(event.role = event2.role) then true
        else loop @@ Id.of_int (Id.to_int n+1)
      in
      loop @@ Id.of_int 0
    in
    let event = Map.find event_id v in
    match event.status with
    | Ready ->
        let agent = Agent.S.of_role event.role roles agents |> Option.get_exn_or "oops" in
        if Agent.is_double_agent agent && event.efficiency = 0 then false else
        if event_has_prev_ready_same_role_ event_id v then false else
        if has_role event then
          true
        else
          true

    | _ -> false

end
