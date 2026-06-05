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
    let event = {
      role;
      status=Ready;
      num_id;
      text;
      bits;
      item_bits;
      efficiency;
    } in
    print_endline @@ show event;
    event::acc
  )
  []
  Iter.(0 -- (num_events - 1)) |> List.rev

let check_process_event roles agents v = match v.status with
  | Ready ->
      let agent = Agent.S.of_role v.role roles agents |> Option.get_exn_or "oops" in
      if Agent.is_double_agent agent && v.efficiency = 0 then false else
      true

  | _ -> false

