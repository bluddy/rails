open Containers
open Tsdl
module Ndarray = Owl_base_dense_ndarray.Generic

type window = {
  zoom: float;
  inner_w: int;
  inner_h: int;
  renderer: Sdl.renderer;
  window: Sdl.window;
  rect: Sdl.rect; (* For drawing rectangles *)
  rect2: Sdl.rect;
  opt_rect: Sdl.rect option; (* reduce allocation *)
}

let do_hide_cursor = false  (* It's buggy on WSL *)

let get_exn = function
  | Ok x -> x
  | Error(`Msg s) -> failwith s

let create w h ~zoom =
  let out_w = Int.of_float @@ zoom *. Float.of_int w in
  let out_h = Int.of_float @@ zoom *. Float.of_int h in
  let window, renderer =
    match Sdl.init Sdl.Init.video with
    | Error(`Msg e) -> Sdl.log "Init error: %s" e; exit 1
    | Ok () ->
      match Sdl.create_window_and_renderer ~w:out_w ~h:out_h Sdl.Window.opengl with
      | Error(`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
      | Ok (w,r) -> w,r
  in
  let hide_cursor () =
    match Sdl.show_cursor false with
    | Ok x -> print_endline @@ Printf.sprintf "set show cursor: %b" x
    | _ -> print_endline "Failed to hide cursor"
  in
  if do_hide_cursor then hide_cursor ();
  Sdl.set_window_grab window true;
  Sdl.render_set_scale renderer zoom zoom |> get_exn;
  let rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0 in
  let rect2 = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0 in
  Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend |> get_exn;
  { inner_w=w; inner_h=h; renderer; window; zoom; rect; rect2; opt_rect=Some rect; }

let zoom _win x = x
  (* win.zoom *. Float.of_int x |> Int.of_float *)

let height window = window.inner_h
let width window = window.inner_w

module Texture = struct
  type t = {
    h: int;
    w: int;
    texture: Sdl.texture;
    surface: Sdl.surface;
    mutable ndarray: (int, Bigarray.int8_unsigned_elt) Sdl.bigarray;
    dst: Sdl.rect;
    mutable dirty_rect: bool;
  }

  let get_w t = t.w
  let get_h t = t.h

  let make win (arr:Pic.ndarray) =
    let h, w = Ndarray.nth_dim arr 0, Ndarray.nth_dim arr 1 in
    let ndarray = Bigarray.reshape_1 arr (w*h*4) in
    let surface =
      Sdl.create_rgb_surface_with_format_from ndarray
        ~w ~h ~depth:32 ~pitch:(w*4) Sdl.Pixel.format_abgr8888
      |> get_exn
    in
    let texture = Sdl.create_texture_from_surface win.renderer surface
      |> get_exn
    in
    let w' = zoom win w in
    let h' = zoom win h in
    let dst = Sdl.Rect.create ~x:0 ~y:0 ~w:w' ~h:h' in
    { w; h; ndarray; texture; surface; dst; dirty_rect=true}

  let destroy tex =
    Sdl.destroy_texture tex.texture

    (* slowish *)
  let update (tex:t) (ndarray:Pic.ndarray) =
    let h, w = Ndarray.nth_dim ndarray 0, Ndarray.nth_dim ndarray 1 in
    let ndarray = (Bigarray.reshape_1 ndarray (w*h*4)) in
    Sdl.update_texture tex.texture None ndarray (tex.w * 4)
      |> get_exn

  let render ?color ~x ~y win tex =
    Sdl.Rect.set_x tex.dst @@ zoom win x;
    Sdl.Rect.set_y tex.dst @@ zoom win y;
    let () = match color with
      | Some (r,g,b,_) ->
          Sdl.set_texture_color_mod tex.texture r g b |> get_exn
      | _ -> ()
    in
    Sdl.render_copy win.renderer tex.texture ~dst:tex.dst |> get_exn

    (* Render only a part of the texture *)
    (* Use different rects so we don't disturb the texture's w and h *)
  let render_subtex ?w ?h ?(from_x=0) ?(from_y=0) ~x ~y win tex =
    let w = match w with | None -> get_w tex | Some w -> w in
    let h = match h with | None -> get_h tex | Some h -> h in
    Sdl.Rect.set_x win.rect @@ zoom win from_x;
    Sdl.Rect.set_y win.rect @@ zoom win from_y;
    Sdl.Rect.set_w win.rect @@ zoom win w;
    Sdl.Rect.set_h win.rect @@ zoom win h;

    Sdl.Rect.set_x win.rect2 @@ zoom win x;
    Sdl.Rect.set_y win.rect2 @@ zoom win y;
    (* w and h must be the same *)
    Sdl.Rect.set_h win.rect2 @@ zoom win h;
    Sdl.Rect.set_w win.rect2 @@ zoom win w;
    Sdl.render_copy win.renderer tex.texture ~src:win.rect ~dst:win.rect2 |> get_exn

end

let _set_color win color =
  let (r,g,b,a) = color in
  Sdl.set_render_draw_color win.renderer r g b a |> get_exn

let draw_rect win ~x ~y ~w ~h ~color ~fill =
  Sdl.Rect.set_x win.rect @@ zoom win x;
  Sdl.Rect.set_y win.rect @@ zoom win y;
  Sdl.Rect.set_w win.rect @@ zoom win w;
  Sdl.Rect.set_h win.rect @@ zoom win h;
  _set_color win color;
  if fill then
    Sdl.render_fill_rect win.renderer win.opt_rect |> get_exn
  else
    Sdl.render_draw_rect win.renderer win.opt_rect |> get_exn

let paint_screen win ~color =
  draw_rect win ~x:0 ~y:0 ~w:(width win) ~h:(height win) ~color ~fill:true

let draw_point ?color win ~x ~y =
  begin match color with
  | Some color ->
      let (r,g,b,a) = color in
      Sdl.set_render_draw_color win.renderer r g b a |> get_exn;
  | None -> ()
  end;
  Sdl.render_draw_point win.renderer x y |> get_exn

  (* Bresenham's algorithm *)
let draw_line win ~x1 ~y1 ~x2 ~y2 ~color =
  _set_color win color;
  let plot_line_high ~x1 ~y1 ~x2 ~y2 =
    let dx, dy = x2 - x1, y2 - y1 in
    let xi, dx =
      if dx < 0 then
        -1, -dx
      else
        1, dx
    in
    let d = 2 * dx - dy in
    
    let rec loop x y d =
      if y > y2 then ()
      else (
        draw_point win ~x ~y;
        let x, d = 
          if d > 0 then
            x + xi, d + (2 * (dx - dy))
          else
            x, d + 2 * dx
        in
        loop x (y+1) d
      )
    in
    loop x1 y1 d
  in
  let plot_line_low ~x1 ~y1 ~x2 ~y2 =
    let dx, dy = x2 - x1, y2 - y1 in
    let yi, dy =
      if dy < 0 then
        -1, -dy
      else
        1, dy
    in
    let d = 2 * dy - dx in
    
    let rec loop x y d =
      if x > x2 then ()
      else (
        draw_point win ~x ~y;
        let y, d = 
          if d > 0 then
            y + yi, d + (2 * (dy - dx))
          else
            y, d + 2 * dy
        in
        loop (x+1) y d
      )
    in
    loop x1 y1 d
  in
  if abs(y2 - y1) < abs(x2 - x1) then
    let x1, y1, x2, y2 =
      if x1 > x2 then
        x2, y2, x1, y1
      else
        x1, y1, x2, y2
    in
    plot_line_low ~x1 ~y1 ~x2 ~y2
  else
    let x1, y1, x2, y2 =
      if y1 > y2 then
        x2, y2, x1, y1
      else
        x1, y1, x2, y2
    in
    plot_line_high ~x1 ~y1 ~x2 ~y2


(*
let draw_line win ~x1 ~y1 ~x2 ~y2 ~color = 
  let (r,g,b,a) = color in
  Sdl.set_render_draw_color win.renderer r g b a |> get_exn;
  Sdl.render_draw_line win.renderer x1 y1 x2 y2 |> get_exn
  *)

let clear_screen win =
  Sdl.render_clear win.renderer |> get_exn

let draw_cursor win texture =
  let _, (mouse_x, mouse_y) = Sdl.get_mouse_state () in
  (* let win_w, win_h = Sdl.get_window_size win.window in *)
  let mouse_x = (float_of_int mouse_x) /. win.zoom |> int_of_float in
  let mouse_y = (float_of_int mouse_y) /. win.zoom |> int_of_float in
  Texture.render ~x:mouse_x ~y:mouse_y win texture


