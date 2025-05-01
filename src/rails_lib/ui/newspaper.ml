(* Drawing of newspaper *)

open! Containers
module R = Renderer
include Newspaper_d

let show_kind = function
  | FinancialNews -> "Financial\nNews"
  | RailRoadNews -> "Railroad\nNews"
  | LocalNews -> "Local\nNews"

let make (s:State.t) kind ?heading text opponent = {
    opponent;
    kind;
    msgbox=Menu.MsgBox.make_basic ~x:58 ~y:98 ?heading ~fonts:s.fonts s text;
}

let render win (s:State.t) v =
  R.draw_rect win ~x:56 ~y:46 ~w:183 ~h:107 ~color:Ega.white ~fill:true;
  R.draw_rect win ~x:56 ~y:46 ~w:183 ~h:107 ~color:Ega.black ~fill:false;
  Fonts.Render.write_shadow win s.fonts ~color:Ega.gray ~x:62 ~y:54 ~idx:2 @@
    show_kind v.kind;
  let tex = match v.opponent with
    | Some opponent ->
      Hashtbl.find s.textures.opponents opponent
    | None ->
      Hashtbl.find s.textures.misc `Newspaper
  in
  R.Texture.render win ~x:194 ~y:50 tex;
  Menu.MsgBox.render win s v.msgbox;
  ()

let handle_event s v event =
  begin match Menu.modal_handle_event ~is_msgbox:true s v.msgbox event with
  | `Exit -> `Exit
  | _ -> `Stay
  end

