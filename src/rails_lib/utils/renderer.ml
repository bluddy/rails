open Containers
open Tsdl
module Ndarray = Owl_base_dense_ndarray.Generic

type window = {
  zoom_x: float;
  zoom_y: float;
  inner_w: int;
  inner_h: int;
  window: Sdl.window;
  rect: Sdl.rect; (* For drawing rectangles *)
  rect2: Sdl.rect;
  opt_rect: Sdl.rect option; (* reduce allocation. points to rect *)
  offscreen_tex: Sdl.texture option;  (* Used for shader effects. *)
  offscreen_gl_id: int; (* OpenGL id for offscreen texture *)
  shader_prog: Opengl.t option; (* program is an int *)
}

let format = Sdl.Pixel.format_rgba8888

let do_hide_cursor = false  (* It's buggy on WSL *)

let get_exn = function
  | Ok x -> x
  | Error(`Msg s) -> failwith s

let clear_screen win =
  Sdl.render_clear win.renderer |> get_exn

let create ?shader_file w h ~zoom_x ~zoom_y =
  let out_w = Int.of_float @@ zoom_x *. Float.of_int w in
  let out_h = Int.of_float @@ zoom_y *. Float.of_int h in

  Sdl.init Sdl.Init.video |> get_exn;
  Sdl.gl_set_attribute Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core |> ignore;
  Sdl.gl_set_attribute Sdl.Gl.context_major_version 3 |> ignore;
  Sdl.gl_set_attribute Sdl.Gl.context_minor_version 3 |> ignore;

  let window = Sdl.create_window ~w:out_w ~h:out_h "Open Railroad Tycoon" Sdl.Window.(opengl + shown) |> get_exn in
  let _ctx = Sdl.gl_create_context window |> get_exn in
  Sdl.gl_set_swap_interval 1 |> ignore;

  let s = match shader_file with None -> "No shader file. Default render" | Some f -> "Using shader file "^f in
  print_endline s;
  let shader_prog = Option.map Opengl.create shader_file in
  let hide_cursor () =
    match Sdl.show_cursor false with
    | Ok x -> print_endline @@ Printf.sprintf "set show cursor: %b" x
    | _ -> print_endline "Failed to hide cursor"
  in
  if do_hide_cursor then hide_cursor ();
  Sdl.set_window_grab window true;
  let rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0 in
  let rect2 = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0 in

  let framebuffer_tex = Opengl.create_texture w h in
  let transition_tex  = Opengl.create_streaming_texture w h in
  {
    inner_w=w;
    inner_h=h;
    window;
    zoom_x;
    zoom_y;
    rect;
    rect2;
    opt_rect=Some rect;
    opengl_state;
    framebuffer_tex;
    transition_tex;
  }

let zoom _win x = x
  (* win.zoom *. Float.of_int x |> Int.of_float *)

let height window = window.inner_h
let width window = window.inner_w

module Transition = struct

type t = {
  w: int; h: int; 
  offscreen_tex: Sdl.texture option;  (* Used for transition effects. option for efficiency *)
  pixels: (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t; (* Copy from render to do transition *)
  tex: Sdl.texture; (* transition texture *)
  rect: Sdl.rect;
  mutable offsets: int list; (* offsets into screen *)
}

let make win random =
  let w, h = win.inner_w, win.inner_w in
  let offscreen_tex  = Tgl3.Gl.create_streaming_texture w h in
  let pixels = Bigarray.Array1.(create Bigarray.int32 Bigarray.c_layout (h*w)) in
  let tex = Sdl.create_texture r format Sdl.Texture.access_streaming ~w ~h |> get_exn in 
  let offsets = Iter.(0 -- (h * w - 1)) |> Iter.to_array in
  Array.shuffle_with random offsets;
  let offsets = Array.to_list offsets in
  (* Create rect for zooming to screen size *)
  let w' = zoom win w in
  let h' = zoom win h in
  let rect = Sdl.Rect.create ~x:0 ~y:0 ~w:w' ~h:h' in
  {w; h; offscreen_tex; pixels; tex; offsets; rect}

let lock_write write_fn v  =
  let open Result in
  match Sdl.lock_texture v.tex None Bigarray.int32 with
  | Error (`Msg str) -> failwith str
  | Ok (dest_buf, pitch) ->
    let x = write_fn dest_buf pitch in
    Sdl.unlock_texture v.tex;
    x

let copy_pixels_to_tex v =
  lock_write (fun buf pitch ->
    for i = 0 to v.h - 1 do
      for j = 0 to v.w - 1 do
        let pixel = Bigarray.Array1.get v.pixels (i * v.w + j) in
        Bigarray.Array1.set buf (i * pitch + j) pixel;
      done
    done)
  v

let render_offscreen win old_render_fn render_fn v =
  (* Do once with final transition image. Render offscreen the next image to our texture. *)
  let old_tgt = Sdl.get_render_target win.renderer in
  Sdl.set_render_target win.renderer v.offscreen_tex |> get_exn;

  (* Old image *)
  old_render_fn win;
  (* Read from texture target to a buffer we can read from *)
  Sdl.render_read_pixels win.renderer None (Some format) v.pixels (win.inner_w * 4) |> get_exn;
  (* Copy all pixels to our streaming texture *)
  copy_pixels_to_tex v;

  (* New image *)
  render_fn win;
  (* Read from texture target to a buffer we can read from *)
  Sdl.render_read_pixels win.renderer None (Some format) v.pixels (win.inner_w * 4) |> get_exn;
  (* Restore render target to the main screen *)
  Sdl.set_render_target win.renderer old_tgt |> get_exn

let step num_pixels v =
  lock_write (fun buf pitch ->
    let rec loop n =
      if n = 0 then `NotDone else
      match v.offsets with
      | [] -> `Done
      | i::_is ->
        v.offsets <- _is;
        let pixel = Bigarray.Array1.get v.pixels i in
        let row, col = i / v.w, i mod v.w in
        let dest_i = row * pitch + col in
        Bigarray.Array1.set buf dest_i pixel;
        loop (n - 1)
    in
    loop num_pixels)
  v

let render win v =
    clear_screen win;
    Sdl.render_copy win.renderer v.tex ~dst:v.rect |> get_exn

end

module Texture = struct
  type t = {
    h: int;
    w: int;
    texture: Sdl.texture;
    mutable ndarray: (int, Bigarray.int8_unsigned_elt) Sdl.bigarray;
    dst: Sdl.rect;
    mutable dirty_rect: bool;
  }

  let get_w t = t.w
  let get_h t = t.h

  let make win (arr:Pic.ndarray) =
    let h, w = Ndarray.nth_dim arr 0, Ndarray.nth_dim arr 1 in
    let ndarray = Bigarray.reshape_1 arr (w*h*4) in
    let texture = Sdl.create_texture win.renderer Sdl.Pixel.format_rgba8888 Sdl.Texture.access_static ~w ~h |> get_exn in
    Sdl.update_texture texture None ndarray (w*4) |> get_exn;
    Sdl.set_texture_blend_mode texture Sdl.Blend.mode_blend |> get_exn;
    let w' = zoom win w in
    let h' = zoom win h in
    let dst = Sdl.Rect.create ~x:0 ~y:0 ~w:w' ~h:h' in
    { w; h; ndarray; texture; dst; dirty_rect=true}

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

let draw_cursor win texture =
  let _, (mouse_x, mouse_y) = Sdl.get_mouse_state () in
  (* let win_w, win_h = Sdl.get_window_size win.window in *)
  let mouse_x = (float_of_int mouse_x) /. win.zoom_x |> int_of_float in
  let mouse_y = (float_of_int mouse_y) /. win.zoom_y |> int_of_float in
  Texture.render ~x:mouse_x ~y:mouse_y win texture

(* New: Render to offscreen texture *)
let render_to_texture win f =
  let old_tgt = Sdl.get_render_target win.renderer in
  Sdl.set_render_target win.renderer win.offscreen_tex |> get_exn;
  f ();
  Sdl.set_render_target win.renderer old_tgt |> get_exn

(* Modified render: Your main loop entry *)
let render_wrap win f x =
  match win.shader_prog with
  | None ->
      (* No shader: original flow *)
      Sdl.render_clear win.renderer |> get_exn;
      f x;
      Sdl.render_present win.renderer;

  | Some state ->
      (* Draw to offscreen, render quad *)
      render_to_texture win (fun () ->
        Sdl.render_clear win.renderer |> get_exn;
        f x;
      );
      Opengl.draw_quad_with_tex state win.offscreen_gl_id win.window ~inner_w:win.inner_w ~inner_h:win.inner_h

