open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers

module Name = struct
let v = [
  "Ford Escort #"; "Chevy Nova #"; "Toyota Tercel #";
  "VW Golf #"; "Mazda RX7 #"; "Honda Civic #";
  "Peugeot #"; (* OG is Peugot *) "Chrysler #"; "Walther PPK #";
  "Smith&Wesson 9mm #"; "Berreta M92S #"; "Browning 9mm #";
  "Hechler&Koch VP #"; "Luger 9mm #"; "Colt 45 #";
  "357 Magnum #"; "Rue Laverne "; "HochStrasse ";
  "Main St. "; "Webley Ct. "; "Holmley Park ";
  "ObensGrabbe "; "La Paz Blvd. "; "North Ave. ";
  "PanAm #"; "United #"; "Sabena #";
  "Lufthansa #"; "El Al #"; "KLM #";
  "SwissAir #"; "BOAC #"; "W.Union #";
  "RCA #"; "Telex #"; "AmTel #"; "InterTel #";
  "SatComm #"; "TelTron #"; "Comlink #";
  ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; "";
  "Swiss Passport #"; "InterPol ID#";
  "Bahamian Passport #"; "Gold Card #"; "Swedish Passport #";
  "Green Card #"; "VISTA Card #"; "Liberian Passport #";
] |> Array.of_list

let get_text id =
  let bottom = id lsr 8 in
  let offset = ((id lsr 4) land 0x7) lsl 1 + (bottom land 0xF) lsl 4 in
  let num = id land 0x3F + 1 in
  let suffix = match bottom land 0xF with
    | 5 -> "00"
    | 6 -> "000"
    | _ -> ""
  in
  Printf.sprintf "%s%d%s" (v.(offset)) num suffix
end

module Connect = struct
  type t =
    | Face of Agent_d.Id.t
    | Agent of Agent_d.Id.t
    | Org of Org.Id.t
    | Loc of Loc.Id.t
    | Role of Role_d.Id.t
    [@@deriving yojson, eq]

  let to_enum = function
    | Face _ -> 0
    | Agent _ -> 1
    | Org _ -> 2
    | Loc _ -> 3
    | Role _ -> 4

  let (=) = equal
  let (<>) x y = not @@ equal x y
end

module Connect_word = struct
  let v = [
    "tied to"; "registered to"; "acquired by";
    "delivered to"; "requested for"; "tied to";
    "registered to"; "traced to"; "smuggled into";
    "requested for"; "tied to"; "rented by";
    "frequented by"; "is in"; "linked to";
    "purchased by"; "purchased by"; "traced to";
    "purchased in"; "linked to"; "received by";
    "signed for by"; "traced to"; "sent to";
    "refers to"; "withdrawn by"; "withdrawn by";
    "paid by"; "transaction in"; "requested for";
    "withdrawn by"; "withdrawn by"; "paid by";
    "transaction in"; "requested for"; "assigned to";
    "assigned to"; "assigned to"; "used in";
  ] |> Array.of_list

  let get_text id connect =
    let clue_val = ((id lsr 8) land 7) * 5 in
    let idx = clue_val + Connect.to_enum connect in
    v.(idx)

end

module Method = struct

type t =
  | Photo
  | Wiretap
  | Surveillance
  | File_search
  | Local_informant
  | Interpol_db
  | Local_authorities
  [@@deriving enum, yojson]

let show = function
  | Photo -> "Clandestine Photo"
  | Wiretap -> "Electronic Wiretap"
  | Surveillance -> "Covert Surveillance"
  | File_search -> "File Record Search"
  | Local_informant -> "Local Informant"
  | Interpol_db -> "INTERPOL Database" (* Data Base in OG *)
  | Local_authorities -> "Local Authorities"

let list = Iter.map (fun i -> of_enum i |> Option.get_exn_or "oops")
  Iter.(0 -- (to_enum Local_authorities)) |> Iter.to_list

end

module Source = struct
  type t =
    | Information
    | Wiretap
    | Files
    | Wall_safe
    | Floor_safe
    [@@deriving yojson, eq]
end

type t = {
  org: Org.Id.t;
  loc: Loc.Id.t;
  role: Role.Id.t;
  connect: Connect.t;
  id: int; (* index into name offset *)
  src: Source.t;
  (* discovery_val: int; seems unused *)
} [@@deriving yojson]

module Id = Engine.Int_id.Make()

module Map = struct
  include Utils.Map.Make(struct 
    type t = Id.t [@@deriving yojson]
    let compare = Id.compare
  end)
  let num v = try (max_binding v |> fst |> Id.to_int) + 1 with Not_found -> 0
end

type map = t Map.t [@@deriving yojson]

