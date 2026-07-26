open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers

type rcv = {
  rcv_agent: Agent.Id.t;
  rcv_loc: Loc.Id.t;
} [@@deriving yojson]

type send = {
  send_agent: Agent.Id.t;
  status: Agent.status;
  send_loc: Loc.Id.t;
  rcv: rcv option;
} [@@deriving yojson]

(* Used only for initial value *)
let default_send = {
  send_agent=Agent.Id.of_int @@ -1;
  status=Arrested;
  send_loc=Loc.Id.of_int @@ -1;
  rcv=None;
}

type kind =
  | Event_based of Event.Id.t * send
  | Travel of Loc.Id.t
  | Break_in of Org.Id.t * Loc.Id.t
  | Item_confiscate of Item.Id.t
  | Item_spotted of Item.Id.t * Loc.Id.t
  | Agent_turn of Agent.Id.t
  | Agent_hide of Agent.Id.t
  | Agent_arrest of Agent.Id.t
  | Agent_leave of Agent.Id.t * Loc.Id.t
  | Agent_escape of Agent.Id.t
  | Agent_exchange of Agent.Id.t
  | Agent_out_of_hiding of Agent.Id.t
  [@@deriving yojson]

type known = [
  | `Known_time
  | `Known_name
  | `Known_org
  | `Known_loc
] [@@deriving yojson, ord, enum]

let to_base2 v = 1 lsl (known_to_enum v)

module KnownSet = struct
  include Utils.Set.Make(struct
    type t = known [@@deriving yojson, ord]
  end)

  let all = [`Known_time; `Known_name; `Known_org; `Known_loc] |> of_list

  let to_base2 v = fold (fun x acc -> acc + to_base2 x) v 0

  let to_discover_val v =
    let base2 = to_base2 v in
    Known_data.clue_discover_vals.(base2)
end

type t = {
  kind: kind;
  time: int;
  known: KnownSet.t;
  decoded: bool;
} [@@deriving yojson]

module Id = Engine.Int_id.Make()

module Map = Utils.Map.Make(struct
  type t = Id.t [@@deriving yojson, ord]
end)

type map = t Map.t [@@deriving yojson]

let create time kind events roles (agents:Agent.map) =
  let time = time.Time.minutes in
  let kind = match kind with
    | Event_based (event_id, _) ->
        let send =
          let agent_id = Event.S.to_role events event_id |> Role.S.to_agent roles in
          let agent = Agent.Map.find agent_id agents in
          let send_loc = agent.loc in
          let status = agent.status in
          let event = Event.Map.find event_id events in
          let rcv = match event.kind with
            | With_role {rcv_role;_} ->
                let rcv_agent_id = Role.S.to_agent roles rcv_role in
                let rcv_agent = Agent.Map.find rcv_agent_id agents in
                let rcv_loc = rcv_agent.loc in
                Some {rcv_agent=rcv_agent_id; rcv_loc}
            | _ -> None
          in
          {send_agent=agent_id; status; send_loc; rcv}
        in
        Event_based (event_id, send)
    | _ -> kind
  in
  {kind; time; known=KnownSet.empty; decoded=false}

let send_loc_eq_rcv_loc v =
  match v.kind with
  | Event_based (_, send) -> begin match send.rcv with
    | Some rcv -> Loc.Id.(send.send_loc = rcv.rcv_loc)
    | _ -> false
    end
  | _ -> false

let add_known known v = {v with known=KnownSet.add known v.known}

let add_known l v = List.fold_left (fun acc x -> add_known x acc) v l

module G = struct
  let send_loc v = match v.kind with
    | Event_based (_, s) -> s.send_loc
    | _ -> failwith "no send loc"

  let send_agent v = match v.kind with
    | Event_based (_, s) -> Some s.send_agent
    | _ -> None

  let rcv_loc v = match v.kind with
    | Event_based (_, s) -> begin match s.rcv with
      | Some r -> r.rcv_loc
      | None -> failwith "no rcv loc"
    end
    | _ -> failwith "no send loc"

  let rcv agent = match agent.kind with
    | Event_based (_, s) -> s.rcv
    | _ -> None
end

module U = struct
  let known_all v = {v with known=KnownSet.all}
end

module S = struct

  let create time kind events roles agents v =
    let action = create time kind events roles agents in
    let id = Map.cardinal v |> Id.of_int in
    Map.add id action v

  let num v = try (Map.max_binding v |> fst |> Id.to_int) + 1 with Not_found -> 0

  let update action_id fn actions =
    Map.update action_id (Option.map fn) actions

  let add_known l action_id actions = update action_id (add_known l) actions

  (* let print_summary_event_based action_id actions = *)
  (*   let action = Map.find action_id actions in *)
  (*   match action.kind with *)
  (*   | Event_based event_id -> () *)
  (*   | _ -> () *)
end

