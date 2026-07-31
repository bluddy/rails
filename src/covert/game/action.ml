open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers

type event_based = {
  status: Agent.status;
  agent1: Agent.Id.t; (* relates to pair's action info *)
  loc1: Loc.Id.t;
  agent2: Agent.Id.t; (* relates to this action info *)
  loc2: Loc.Id.t;
} [@@deriving yojson]

(* Used only for initial value *)
let default_send = {
  status=Arrested;
  agent1=Agent.Id.of_int @@ -1;
  loc1=Loc.Id.of_int @@ -1;
  agent2=Agent.Id.of_int @@ -1;
  loc2=Loc.Id.of_int @@ -1;
}

type kind =
  | Event_based of Event.Id.t * event_based
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

module Known = struct

type t = [
  | `Known_time
  | `Known_agent
  | `Known_org
  | `Known_loc
] [@@deriving yojson, ord, enum]

  let to_base2 v = 1 lsl (to_enum v)

  let all = [`Known_time; `Known_agent; `Known_org; `Known_loc]

  let random r = Random.choose_return all r

  module Set = struct
    include Utils.Set.Make(struct
      type known = t [@@deriving yojson, ord]
      type t = known [@@deriving yojson, ord]
    end)

    let all = all |> of_list

    let to_base2 v = fold (fun x acc -> acc + to_base2 x) v 0

    let to_discover_val v =
      let base2 = to_base2 v in
      Known_data.clue_discover_vals.(base2)

  end
end

type t = {
  kind: kind;
  time: int;
  known: Known.Set.t;
  decoded: bool;
} [@@deriving yojson]

module Id = struct
  include Engine.Int_id.Make()
  let prev v = (to_int v) - 1 |> of_int
  let next v = (to_int v) + 1 |> of_int
end

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
          let loc1 = agent.loc in
          let status = agent.status in
          let event = Event.Map.find event_id events in
          let agent2_id, loc2 = match event.kind with
            | With_role {rcv_role;_} ->
                let agent2_id = Role.S.to_agent roles rcv_role in
                let agent2 = Agent.Map.find agent2_id agents in
                let loc2 = agent2.loc in
                agent2_id, loc2
            | _ -> failwith "Invalid event for event-based action"
          in
          {agent1=agent_id; status; loc1; loc2; agent2=agent2_id}
        in
        Event_based (event_id, send)
    | _ -> kind
  in
  {kind; time; known=Known.Set.empty; decoded=false}

let loc1_eq_loc2 v =
  match v.kind with
  | Event_based (_, send) -> Loc.Id.(send.loc1 = send.loc2)
  | _ -> false

let add_known known v = {v with known=Known.Set.add known v.known}

let add_known l v = List.fold_left (fun acc x -> add_known x acc) v l

let is_known k v = Known.Set.mem k v.known

let is_known_all l v = Known.Set.mem_all l v.known

module G = struct
  let known v = v.known
  let loc1 v = match v.kind with
    | Event_based (_, s) -> Some s.loc1
    | _ -> None

  let agent1 v = match v.kind with
    | Event_based (_, s) -> Some s.agent1
    | _ -> None

  let loc2 v = match v.kind with
    | Event_based (_, s) -> Some s.loc2
    | _ -> None

  let agent2 agent = match agent.kind with
    | Event_based (_, s) -> Some s.agent2
    | _ -> None
end

module U = struct
  let known_all v = {v with known=Known.Set.all}
end

module S = struct

  let create time kind events roles agents v =
    let action = create time kind events roles agents in
    let id = Map.cardinal v |> Id.of_int in
    Map.add id action v

  let num v = try (Map.max_binding v |> fst |> Id.to_int) + 1 with Not_found -> 0

  let update action_id fn v =
    Map.update action_id (Option.map fn) v

  let with_action action_id fn v =
    let action = Map.find action_id v in
    fn action

  module G = struct
    let known action_id v = with_action action_id G.known v
    let loc2 action_id v = with_action action_id G.loc2 v
  end

  let add_known l action_id actions = update action_id (add_known l) actions

  let is_known known action_id v =
    with_action action_id (is_known known) v

  let is_known_all known action_id v =
    with_action action_id (is_known_all known) v

  (* Find the next or prev entry with the same time *)
  let same_time_idx action_id v =
    let time = (Map.find action_id v).time in
    let prev_id, next_id = Id.prev action_id, Id.next action_id in
    let other_id = match Map.get prev_id v with
      | Some x when x.time = time -> Some prev_id
      | _ -> match Map.get next_id v with
          | Some x when x.time = time -> Some next_id
          | _ -> None
    in
    other_id

end

