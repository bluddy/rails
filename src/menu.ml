open Containers

type 'a entry =
  | Toggle of
    {
      check: bool;
      name: string;
      v: 'a;
    }
  | Entry of
    {
      name: string;
      v: 'a;
    }

type 'a options =
  | Exclusive of 'a entry list
  | Entries of 'a entry list

type 'a dropdown = {
  x: int;
  y: int;
  options: 'a options;
}

let dropdown_empty = {x=0; y=0; options=Entries []}

(* in the upper bar *)
type 'a single = {
  x: int;
  y: int;
  w: int;
  h: int;
  name: string;
  dropdown: 'a dropdown;
}

let make_single ~fonts ~name ~x ~y ~dropdown =
  let w, h = Fonts.get_str_w_h ~fonts ~idx:1 name in
  let dropdown = Option.get_or dropdown ~default:dropdown_empty in
  {
    x; y;
    w; h;
    name;
    dropdown;
  }

type 'a global = {
  menu_open: int option;
  menus: 'a single list;
}

let default = {
  menu_open=None;
  menus=[];
}

let update v (event:Event.t) =
  match event with
  | MouseButton {down=true; x; y; _} -> v


let render win fonts v =
  List.iter (fun menu ->
    String.fold (fun (x, y, key) c ->
      if Char.Infix.(c = '&') then
        (x, y, true)
      else
        let color = if key then Ega.white else Ega.bcyan in
        let (x, y) = Fonts.Render.write_char win fonts c ~idx:1 ~x ~y ~color in
        (x, y, false))
    (menu.x, menu.y, false)
    menu.name
    |> ignore
  )
  v.menus



