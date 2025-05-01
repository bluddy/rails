open! Containers

type kind =
  | FinancialNews
  | RailRoadNews
  | LocalNews

type 'state t = {
  opponent: Opponent.name option;
  kind: kind;
  msgbox: (unit, 'state) Menu.MsgBox.t;
}
