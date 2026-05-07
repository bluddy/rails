open! Containers

type status =
  | Arrested
  | In_hiding
  | Double_agent
  | Escaped
  | Exchanged
  | Out_of_hiding
  | At_large
  | In_custody

type t = {
  sex: Gender.t;
  org: Org.Id.t;
  first_name: string;
  last_name: string;
  id_code: int;
  rank: string;
  loc: Loc.Id.t;
  known: Known_data.t;
  role: int;
  status: status;
  anxiety: int;
}

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
  ]

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
  ]

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
  ]

end

let name_rank_of_code region x =
  let sex = x land 1 in
  let name_idx = (x lsr 1) land 0xF in
  let last_name_idx = (x lsr 5) land 0xF in
  ()


