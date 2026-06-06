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
    let test_add bit prop kind =
      if bits land bit > 0 then Kind.Set.add prop kind else kind
    in
    let kind = Kind.Set.empty
    |> test_add 0x100 Kind.Rcv_msg
    |> test_add 0x200 Kind.Sent_msg
    |> test_add 0x400 Kind.Send_package
    |> test_add 0x800 Kind.Meeting
    |> test_add 0x1000 Kind.Use_anxiety
    |> test_add 0x2000 Kind.Misc_action
    |> test_add 0x8000 Kind.Event_known
    in
    let rcv_role = bits land 0xFF in
    let rcv_role = if rcv_role = 0 then None else Some (Role.Id.of_int rcv_role) in
    let event = {
      role;
      status=Ready;
      num_id;
      text;
      bits;
      item_bits;
      efficiency;
      kind;
      rcv_role;
    } in
    print_endline @@ (yojson_of_t event |> Yojson.Safe.to_string);
    event::acc
  )
  []
  Iter.(0 -- (num_events - 1)) |> List.rev

let is_ready v = match v.status with Ready -> true | _ -> false

module S = struct
  let event_has_prev_ready_same_role_ event_id v =
    let event = Map.find event_id v in
    let rec loop n =
      if Id.(event_id = n) then false else
      let event2 = Map.find n v in
      if is_ready event2 && Role.Id.(event.role = event2.role) then true
      else loop @@ Id.of_int (Id.to_int n+1)
    in
    loop @@ Id.of_int 0

  let check_process_event event_id roles agents v =
    let event = Map.find event_id v in
    match event.status with
    | Ready ->
        let agent = Agent.S.of_role event.role roles agents |> Option.get_exn_or "oops" in
        if Agent.is_double_agent agent && event.efficiency = 0 then false else
        if event_has_prev_ready_same_role_ event_id v then false else

        true

    | _ -> false

end
