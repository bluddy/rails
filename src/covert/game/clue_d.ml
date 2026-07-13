open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers

let names = [
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

module Connect = struct
  type t =
    | Face of Agent_d.Id.t
    | Agent of Agent_d.Id.t
    | Org of Org.Id.t
    | Loc of Loc.Id.t
    | Role of Role_d.Id.t
    [@@deriving yojson]

  let to_enum = function
    | Face _ -> 0
    | Agent _ -> 1
    | Org _ -> 2
    | Loc _ -> 3
    | Role _ -> 4
end

type t = {
  org: Org.Id.t;
  loc: Loc.Id.t;
  role: Role.Id.t;
  connect: Connect.t;
  name_idx: int; (* index into name offset *)
  (* discovery_val: int; seems unused *)
} [@@deriving yojson]

module Id = Engine.Int_id.Make()

module Map = struct
  include Utils.Map.Make(struct 
    type t = Id.t [@@deriving yojson]
    let compare = Id.compare
  end)
end

type map = t Map.t [@@deriving yojson]

