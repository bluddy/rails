open Containers

module R = Renderer

type cursor_flash = {
  delta: int;
  mutable last_time: int;
  mutable active: bool
}

type t = {
  cursor: int option;
  text: string;
  x: int;
  y: int;
  num_chars: int;
  font_idx: int;
  text_color: Ega.color;
  cursor_color: Ega.color;
  cursor_height: int;
  frame_color: Ega.color option;
  cursor_flash: cursor_flash option;
}

let char_width = 8
let char_height = 8

(* cursor_height: pixels below chars *)
let make 
  ?(editable=true)
  ?(font_idx=4)
  ?(text_color=Ega.black) 
  ?(cursor_color=Ega.bcyan)
  ?(frame_color=Some Ega.black)
  ?(cursor_height=2)
  ?cursor_flash
  text ~x ~y ~chars =
  let cursor = if editable then Some 0 else None in
  let len = String.length text in
  let num_spaces = max 0 (chars - len) in
  let text = text ^ String.make num_spaces ' ' in
  let cursor_flash = Option.map (fun time -> {delta=time; active=true; last_time=0}) cursor_flash in
  {
    cursor; text;
    x; y;
    num_chars=chars;
    font_idx; text_color;
    cursor_color; cursor_height;
    frame_color; cursor_flash;
  }

let get_text v = String.rdrop_while (function ' ' -> true | _ -> false) v.text

let render win fonts v =
  (* draw frame *)
  Option.iter (fun color ->
    let w = v.num_chars * char_width + 2 + 2 in
    let h = 8 + 2 + 3 in
    R.draw_rect win ~x:v.x ~y:v.y ~w ~h ~color ~fill:false;
  ) v.frame_color;

  (* draw text *)
  let y = v.y + 2 in
  let x = v.x + 2 in
  match v.cursor, v.cursor_flash with
  | Some index, (Some {active=true;_} | None) ->
    let cursor = Fonts.{index; color=v.cursor_color; cur_height= v.cursor_height} in
    Fonts.Render.write win fonts v.text ~cursor ~color:v.text_color ~idx:v.font_idx ~x ~y
  | _ ->
    Fonts.Render.write win fonts v.text ~color:v.text_color ~idx:v.font_idx ~x ~y

let handle_event v event =
  match v.cursor with
  | None -> v, `Stay
  | Some cursor ->
    begin match event with
    | Event.Key ({down=true; key; _} as keyinfo) ->
      let insert_char c v =
        let text1, text2 = String.take cursor v.text, String.drop (cursor + 1) v.text in
        Printf.sprintf "%s%c%s" text1 c text2
      in
      begin match key with
      | _ when Event.is_alphanumeric key && cursor < v.num_chars ->
        let shift = Event.Modifiers.shift keyinfo.modifiers in
        let c = Event.char_of_key ~shift key in
        let text = insert_char c v in
        {v with text; cursor=Some(cursor + 1)}, `Stay
      | Space when cursor < v.num_chars ->
        let text = insert_char ' ' v in
        {v with text; cursor=Some(cursor + 1)}, `Stay
      | Left when cursor > 0 ->
        {v with cursor=Some(cursor - 1)}, `Stay
      | Right when cursor < String.length v.text - 1 ->
        {v with cursor=Some(cursor + 1)}, `Stay
      | Backspace when cursor > 0 ->
        let text1, text2 = String.take (cursor-1) v.text, String.drop cursor v.text in
        let text = text1 ^ text2 ^ " " in
        {v with cursor=Some(cursor-1); text}, `Stay
      | Enter -> v, `Return(get_text v)
      | Escape -> v, `Exit
      | _ -> v, `Stay
      end
    | _ -> v, `Stay
    end

let handle_tick time v =
  Option.iter (fun t ->
    if time - t.last_time > t.delta then
      begin
        t.active <- not t.active;
        t.last_time <- time;
      end
    )
    v.cursor_flash;
    v, `Stay


let set_writeable x v = match x, v.cursor with
  | true, None -> {v with cursor=Some 0}
  | false, Some _ -> {v with cursor=None}
  | _ -> v

