open Containers

type dims = {
  menu_h: int;
  ui_w: int;
  ui_start_x: int;
  minimap_h: int;
  infobar_h: int;
  train_area_h: int;
  width: int;
  height: int;
}

type t = {
  dims: dims;
}

module R = Renderer

let default win =
  let dims =
    {
      menu_h=8;
      minimap_h=55;
      width=R.width win;
      height=R.height win;
      ui_w=64;
      ui_start_x=Gmap.map_width-1;
      infobar_h=19;
      train_area_h=115;
    }
  in
  {dims}

let mapview_start v = v.dims.menu_h

let render win v =
  let dims = v.dims in
  (* Menu bar *)
  R.draw_rect win ~x:0 ~y:0 ~w:dims.width ~h:dims.menu_h ~color:Ega.cyan ~fill:true;
  let h = dims.height - dims.menu_h in
  let y = dims.menu_h in

  (* Screen White border *)
  R.draw_rect win ~x:0 ~y ~w:dims.width ~h ~color:Ega.white ~fill:false;

  let x = dims.ui_start_x in

  (* Border of UI *)
  R.draw_rect win ~x ~y ~h ~w:(dims.ui_w+1) ~color:Ega.white ~fill:false;

  (* Info bar *)
  let y = y + dims.minimap_h in
  R.draw_rect win ~x ~y ~h:dims.infobar_h ~w:dims.ui_w ~color:Ega.white ~fill:true;

  (* Train area *)
  let y = y + dims.infobar_h in
  (* R.draw_rect win ~x:(x+1) ~y:(y+1) ~h:dims.train_area_h ~w:dims.ui_w ~color:Ega.bblue ~fill:true; *)
  R.draw_rect win ~x:(x+1) ~y:(y+1) ~h:dims.train_area_h ~w:(dims.ui_w-1) ~color:Ega.bblue ~fill:true;
  ()

