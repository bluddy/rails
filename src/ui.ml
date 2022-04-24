open Containers
open Ui_d

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

let render (win:R.window) (s:State.t) (v:t) ~(draw_logo:bool) : unit =
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

  if draw_logo then 
    R.Texture.render ~x:(x+1) ~y:(y+1) win s.State.textures.Textures.logo;

  (* Info bar *)
  let y = y + dims.minimap_h in
  R.draw_rect win ~x ~y ~h:dims.infobar_h ~w:dims.ui_w ~color:Ega.white ~fill:true;


  (* Train area *)
  let y = y + dims.infobar_h in
  R.draw_rect win ~x:(x+1) ~y:(y+1) ~h:dims.train_area_h ~w:(dims.ui_w-1) ~color:Ega.bblue ~fill:true;
  ()

