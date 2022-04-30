open Containers
open Main_ui_d

module R = Renderer

let main_menu =
  let open Menu in
  let main =
    [
      {
        x=8; y=1;
        name="&Game";
        dropdown=dropdown_empty;
      };
      {
        x=64; y=1;
        name="&Displays";
        dropdown=dropdown_empty;
      };
      {
        x=120; y=1;
        name="&Reports";
        dropdown=dropdown_empty;
      };
      {
        x=176; y=1;
        name="&Build";
        dropdown=dropdown_empty;
      };
      {
        x=242; y=1;
        name="&Actions";
        dropdown=dropdown_empty;
      };
    ]
  in
  { 
    menu_open=None;
    menus=main
  }

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
  {
    dims;
    menu=main_menu;
  }

let update (s:State.t) (v:t) (event:Event.t) =
  let dims = v.dims in
  let minimap_x = dims.ui_start_x in
  let minimap_y = dims.menu_h in
  let minimap_h = dims.minimap_h in
  let minimap_w = dims.ui_w in
  let view, actions =
    Mapview.update s s.view event ~y_top:dims.menu_h ~minimap_x ~minimap_y ~minimap_w ~minimap_h
  in
  v, view, actions
  

let render (win:R.window) (s:State.t) (v:t) =
  let dims = v.dims in
  (* Render main view *)
  let minimap_x = dims.ui_start_x in
  let minimap_y = dims.menu_h in
  let minimap_h = dims.minimap_h in
  let minimap_w = dims.ui_w in
  let s = Mapview.render win s s.view ~y:v.dims.menu_h ~minimap_x ~minimap_y ~minimap_w ~minimap_h in

  (* Menu bar *)
  R.draw_rect win ~x:0 ~y:0 ~w:dims.width ~h:dims.menu_h ~color:Ega.cyan ~fill:true;
  let h = dims.height - dims.menu_h in
  let y = dims.menu_h in

  (* Screen White border *)
  R.draw_rect win ~x:0 ~y ~w:dims.width ~h ~color:Ega.white ~fill:false;

  let x = dims.ui_start_x in

  (* Border of UI *)
  R.draw_rect win ~x ~y ~h ~w:(dims.ui_w+1) ~color:Ega.white ~fill:false;

  begin match Mapview.get_zoom s.view with
  | Zoom1 ->
      R.Texture.render ~x:(x+1) ~y:(y+1) win s.State.textures.Textures.logo;
  | _ ->
      ()
  end;

  (* Info bar *)
  let y = y + dims.minimap_h in
  R.draw_rect win ~x ~y ~h:dims.infobar_h ~w:dims.ui_w ~color:Ega.white ~fill:true;

  (* Train area *)
  let y = y + dims.infobar_h in
  R.draw_rect win ~x:(x+1) ~y:(y+1) ~h:dims.train_area_h ~w:(dims.ui_w-1) ~color:Ega.bblue ~fill:true;

  (* Menu bar *)
  Menu.render win s.textures.fonts v.menu;

  s






