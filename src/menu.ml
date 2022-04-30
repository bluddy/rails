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
  name: string;
  dropdown: 'a dropdown;
}

type 'a global = {
  menu_open: int option;
  menus: 'a single list;
}

let default = {
  menu_open=None;
  menus=[];
}

let render win fonts v =
  List.iter (fun menu ->
    Fonts.Render.write win fonts menu.name ~x:menu.x ~y:menu.y ~color:Ega.bcyan
  )
  v.menus



