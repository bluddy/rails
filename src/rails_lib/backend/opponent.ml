
type t =
  | CorneliusVanderbilt
  | DanielDrew
  | JimFisk
  | JayGould
  | ErastusCorning
  | JPierpontMorgan
  | JEdgarThompson
  | JimHill
  | JayCooke
  | JohnForbes
  | CzarNicholasII
  | VILenin
  | CharlesDeGaulle
  | NapoleonIII
  | OttoVonBismarck
  | BenitoMussolini
  | GeorgeStephenson
  | RobertStephenson
  | IsambardKBrunel
  | GeorgeSHudson
  | HelmuthVonMoltke
  | BaronRothschild
  [@@deriving yojson]

let show = function
  | CorneliusVanderbilt -> "Cornelius Vanderbilt"
  | DanielDrew -> "Daniel Drew"
  | JimFisk -> "Jim Fisk"
  | JayGould -> "Jay Gould"
  | ErastusCorning -> "Erastus Corning"
  | JPierpontMorgan -> "J.Pierpont Morgan"
  | JEdgarThompson -> "J.Edgar Thompson"
  | JimHill -> "Jim Hill"
  | JayCooke -> "Jay Cooke"
  | JohnForbes -> "John Forbes"
  | CzarNicholasII -> "Czar Nicholas II"
  | VILenin -> "V.I. Lenin"
  | CharlesDeGaulle -> "Charles De Gaulle"
  | NapoleonIII -> "Napolean III"
  | OttoVonBismarck -> "Otto von Bismarck"
  | BenitoMussolini -> "Benito Mussolini"
  | GeorgeStephenson -> "George Stephenson"
  | RobertStephenson -> "Robert Stephenson"
  | IsambardKBrunel -> "Isambard K.Brunel"
  | GeorgeSHudson -> "George S.Hudson"
  | HelmuthVonMoltke -> "Helmuth von Moltke"
  | BaronRothschild -> "Baron Rothschild"

