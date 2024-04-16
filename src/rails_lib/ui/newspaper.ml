(* Drawing of newspaper *)

open! Containers
module R = Renderer

type kind =
  | FinancialNews
  | RailRoadNews
  | LocalNews

let show_kind = function
  | FinancialNews -> "Financial\nNews"
  | RailRoadNews -> "Railroad\nNews"
  | LocalNews -> "Local\nNews"

type 'state t = {
  opponent: Opponent.t option;
  kind: kind;
  msgbox: (unit, 'state) Menu.MsgBox.t;
}

let make (s:State.t) kind text opponent = {
    opponent;
    kind;
    msgbox=Menu.MsgBox.make_basic ~x:58 ~y:98 ~fonts:s.fonts s text;
}

let render win (s:State.t) v =
  R.draw_rect win ~x:56 ~y:46 ~w:183 ~h:107 ~color:Ega.white ~fill:true;
  R.draw_rect win ~x:56 ~y:46 ~w:183 ~h:107 ~color:Ega.black ~fill:false;
  Fonts.Render.write_shadow win s.fonts ~color:Ega.dgray ~x:62 ~y:54 ~idx:0 @@
    show_kind v.kind;
  begin match v.opponent with
  | Some opponent ->
    let tex = Hashtbl.find s.textures.opponents opponent in
    R.Texture.render win ~x:194 ~y:50 tex;
  | None -> () (* TODO: newspaper pic? *)
  end;
  Menu.MsgBox.render win s v.msgbox;
  ()

let handle_event v s event =
  begin match Menu.modal_handle_event ~is_msgbox:true s v.msgbox event with
  | `Exit -> `Exit
  | _ -> `Stay
  end

