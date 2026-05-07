open! Containers

type means =
  | Photo
  | Wiretap
  | Surveillance
  | File_search
  | Local_informant
  | Interpol_db
  | Local_authorities
  [@@deriving enum]

let means_list = Iter.map (fun i -> means_of_enum i |> Option.get_exn_or "oops")
  Iter.(0 -- (means_to_enum Local_authorities)) |> Iter.to_list
