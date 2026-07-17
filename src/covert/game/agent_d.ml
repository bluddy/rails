open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers

type status =
  | Arrested
  | In_hiding
  | Double_agent
  | Escaped
  | Exchanged
  | Out_of_hiding
  | At_large of {anxiety: int}
  | In_custody
  [@@deriving yojson]

module Face = struct
  type t = {
    gender: Gender.t;
    head: int; (* 0-1 *)
    mouth: int; (* 0-7 *)
    nose: int;
    eyes: int;
    hair: int;
    neck: int; (* 0-3 *)
    id: int;
  } [@@deriving yojson]

  let of_code code =
    let gender = Gender.of_code code in
    let head = (code lsr 1) land 1 in
    let mouth = (code lsr 4) land 7 in
    let nose = (code lsr 7) land 7 in
    let eyes  = (code lsr 10) land 7 in
    let hair = (code lsr 13) land 7 in
    (* This is a fix to a BUG in the OG: bit 1 is forced by gender!
      We change to shift by the gender bit and head size bit.
    *)
    let neck = (code lsr 2) land 3 in
    {gender; head; mouth; nose; eyes; hair; neck; id=code}
end

type t = {
  face: Face.t;
  org: Org.Id.t;
  name: string;
  last_name: string;
  loc: Loc.Id.t;
  known: Known_data.Set.t;
  roles: Role_d.Set.t;
  roles_known: Role_d.Set.t;
  discover_val: int;
  status: status;
} [@@deriving yojson]

module Id = Agent_id

module Map = Utils.Map.Make(struct
  type t = Id.t [@@deriving yojson, ord]
end)

type map = t Map.t [@@deriving yojson]

