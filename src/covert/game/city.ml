open Containers

type t =
  | Amman
  | Amsterdam
  | Athens
  | Baghdad
  | Beirut
  | Belgrade
  | Berlin
  | Bogota
  | Budapest
  | Cairo
  | Caracas
  | Damascus
  | Geneva
  | Havana
  | Helsinki
  | Istanbul
  | Khartoum
  | Kingston
  | London
  | LosAngeles
  | Madrid
  | Managua
  | Medellin
  | MexicoCity
  | Miami
  | Moscow
  | Nassau
  | Panama
  | Paris
  | Prague
  | Rio
  | Riyadh
  | Rome
  | SanJuan
  | Tehran
  | TelAviv
  | Tripoli
  | Vienna
  | WashingtonDC
  [@@deriving show{with_path=false}, enum]

let iter_all f =
  let open Iter in
  iter (fun city_idx -> of_enum city_idx |> Option.get_exn_or "oops" |> f) @@
  0 -- to_enum WashingtonDC


