open! Containers

type kind =
  | FinancialNews
  | RailRoadNews
  | LocalNews

module Simple = struct

  let show_kind = function
    | FinancialNews -> "Financial\nNews"
    | RailRoadNews -> "Railroad\nNews"
    | LocalNews -> "Local\nNews"

  type 'state t = {
    opponent: Opponent.name option;
    kind: kind;
    msgbox: (unit, 'state) Menu.MsgBox.t;
  }
end

module Fancy = struct
  type source =
    | RailNewsWeekly
    | DailyTattler
    | NewYorkTimes
    | LondonTimes
    | WallStreetJournal
    | HeraldTribune

  let show_source = function
    | RailNewsWeekly -> "RailNews Weekly"
    | DailyTattler -> "The Daily Tattler"
    | NewYorkTimes -> "The New York Times"
    | LondonTimes -> "The Londone Times"
    | WallStreetJournal -> "The Wall Street Journal"
    | HeraldTribune -> "The Herald Tribune"

  let source_pos = function
    | RailNewsWeekly -> (64, 4)
    | DailyTattler -> (88, 4)
    | NewYorkTimes -> (56, 4)
    | LondonTimes -> (64, 4)
    | WallStreetJournal -> (32, 6)
    | HeraldTribune -> (56, 6)

  (* A large newspaper for events *)
  type 'state t = {
    source: source;
    text: string * string * string; (* 3 lines *)
    msgbox: (unit, 'state) Menu.MsgBox.t;
    cost_s: string;
    date_s: string;
    tear_vals: (int * int) list; (* values for random tear *)
  }
end

type 'state t =
  | Simple of 'state Simple.t
  | Fancy of 'state Fancy.t

