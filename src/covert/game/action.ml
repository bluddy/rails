open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers

type kind =
  | Event_based of Event.Id.t
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
  | `Decoded
  | `Known_time
  | `Known_name
  | `Known_org
  | `Known_loc
] [@@deriving yojson, ord]

module KnownSet = struct
  include Utils.Set.Make(struct
  type t = known [@@deriving yojson, ord]
end)

  let all = [`Decoded; `Known_time; `Known_name; `Known_org; `Known_loc]
    |> of_list
end

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

type t = {
  kind: kind;
  time: int;
  known: KnownSet.t;
  send: send option;
  bulletin: bool; (* whether we find out about this *)
} [@@deriving yojson]

module Id = Engine.Int_id.Make()

module Map = Utils.Map.Make(struct
  type t = Id.t [@@deriving yojson, ord]
end)

type map = t Map.t [@@deriving yojson]

let create ?(bulletin=false) time kind events roles (agents:Agent.map) =
  let time = time.Time.minutes in
  let send = match kind with
    | Event_based event_id ->
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
        Some {send_agent=agent_id; status; send_loc; rcv}
    | _ -> None
  in
  {kind; time; known=KnownSet.empty; send; bulletin}

module S = struct

  let create ?bulletin time kind events roles agents v =
    let action = create ?bulletin time kind events roles agents in
    let id = Map.cardinal v |> Id.of_int in
    Map.add id action v

  let num v =
    try (Map.max_binding v |> fst |> Id.to_int) + 1 with Not_found -> 0

  let update action_id actions fn =
    Map.update action_id (Option.map fn) actions
end

module G = struct
  let rcv agent = match agent.send with
    | Some {rcv=Some r; _} -> Some r
    | _ -> None
end
