open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers

module Names = struct
  let female_names = [
    "Georgina";
    "Arkada";
    "Yuria";
    "Anatola";
    "Andra";
    "Nikola";
    "Viktora";
    "Ivana";
    "Natasha";
    "Svetlana";
    "Anna";
    "Ludmilla";
    "Lexandra";
    "Katerina";
    "Olga";
    "Nadia";
    "Leona";
    "Pat";
    "Carla";
    "Bobbie";
    "Jenny";
    "Frieda";
    "Alice";
    "Rose";
    "Deirdre";
    "Martha";
    "Lisa";
    "Erica";
    "Liz";
    "Emma";
    "Isadora";
    "Jane";
    "Omara";
    "Alicia";
    "Hasana";
    "Leila";
    "Yasria";
    "Ailha";
    "Yousfa";
    "Aziza";
    "Fatima";
    "Benazir";
    "Ouida";
    "Selima";
    "Ismaila";
    "Persis";
    "Haurouna";
    "Zara";
    "Carlotta";
    "Josita";
    "Miguela";
    "Ramona";
    "Luisa";
    "Julia";
    "Juanita";
    "Raquel";
    "Isabel";
    "Maria";
    "Elena";
    "Linda";
    "Carmen";
    "Agnes";
    "Celeste";
    "Phyllis";
  ] |> Array.of_list

  let male_names = [
    "Georgi";
    "Arkady";
    "Yuri";
    "Anatol";
    "Andrei";
    "Nikita";
    "Viktor";
    "Ivan";
    "Boris";
    "Nikolai";
    "Grigori";
    "Pavel";
    "Mischa";
    "Petrov";
    "Vladimir";
    "Mikhail";
    "Leon";
    "Ian";
    "Carl";
    "Bob";
    "Jim";
    "Fred";
    "Alan";
    "Ralph";
    "Antoine";
    "Heinrich";
    "Eric";
    "Gordon";
    "David";
    "Pierre";
    "Claude";
    "Gerard";
    "Omar";
    "Ali";
    "Hasan";
    "Abdul";
    "Yasr";
    "Abu";
    "Yousf";
    "Saadi";
    "Achmet";
    "Ibrahim";
    "Selim";
    "Ismail";
    "Mahmoud";
    "Haround";
    "Suleiman";
    "Yezid";
    "Carlos";
    "Jose";
    "Miguel";
    "Ramon";
    "Luis";
    "Julio";
    "Juan";
    "Pedro";
    "Enrique";
    "Eduardo";
    "Arturo";
    "Jorge";
    "Manuel";
    "Hector";
    "Franco";
    "Jaime";
  ] |> Array.of_list

  let last_names = [
    "Katkov";
    "Gorny";
    "Gromyko";
    "Kormarov";
    "Molotov";
    "Charyk";
    "Markov";
    "Filatov";
    "Badenov";
    "Checkhov";
    "Gogol";
    "Zhukov";
    "Nevsky";
    "Kemidov";
    "Dubrovnik";
    "Kerensky";
    "Lorenz";
    "Koch";
    "Baader";
    "Brown";
    "Jones";
    "Bloom";
    "Cahill";
    "Grivas";
    "Cook";
    "Muller";
    "Hansen";
    "Olsen";
    "Merrick";
    "Ballard";
    "Verne";
    "Konrad";
    "Abbas";
    "Said";
    "Daoud";
    "Moussa";
    "Nidal";
    "Khaled";
    "Hammid";
    "Afafat";
    "Agha";
    "Murad";
    "Akbar";
    "Hosayn";
    "Raisuli";
    "Musafir";
    "Nasr";
    "Rashid";
    "Munoz";
    "Diaz";
    "Munoz"; (* double *)
    "Perez";
    "Ramirez";
    "Alvarez";
    "Garcia";
    "Lopez";
    "Santos";
    "Diego";
    "Cordoba";
    "Valdez";
    "Santiago";
    "Enriquez";
    "Sanchez";
    "Delarosa";
  ] |> Array.of_list

end

type status =
  | Arrested
  | In_hiding
  | Double_agent
  | Escaped
  | Exchanged
  | Out_of_hiding
  | At_large
  | In_custody
  [@@deriving yojson]

type t = {
  gender: Gender.t;
  org: Org.Id.t;
  name: string;
  last_name: string;
  id_code: int; (* Used to generate the name, sex and picture *)
  loc: Loc.Id.t;
  known: Known_data.Set.t;
  role: int;
  status: status;
  anxiety: int;
} [@@deriving yojson]

let gender_name_of_code ~name_offset x =
  let gender = x land 1 in
  let name_idx = (x lsr 1) land 0xF in
  let last_name_idx = (x lsr 5) land 0xF in
  let name_arr = if gender = 0 then Names.female_names else Names.male_names in
  let name, last_name =
      name_arr.(name_offset + name_idx),
      Names.last_names.(name_offset + last_name_idx)
  in
  let gender = if gender = 0 then `Female else `Male in
  gender, name, last_name

  (* name_offset comes from org *)
let create ~name_offset id_code org loc =
  let gender, name, last_name = gender_name_of_code ~name_offset id_code in
  {
    gender;
    org;
    name;
    last_name;
    id_code;
    loc;
    known=Known_data.Set.empty;
    role=0;
    status=At_large;
    anxiety=0;
  }

module Id = Engine.Int_id.Make()

