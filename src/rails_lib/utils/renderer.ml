open Containers
open Tsdl
open Tgl3
module Ndarray = Owl_base_dense_ndarray.Generic

type window = {
  zoom_x: float;
  zoom_y: float;
  inner_w: int;
  inner_h: int;
  renderer: Sdl.renderer;
  window: Sdl.window;
  rect: Sdl.rect; (* For drawing rectangles *)
  rect2: Sdl.rect;
  opt_rect: Sdl.rect option; (* reduce allocation. points to rect *)
  offscreen_tex: Sdl.texture option;  (* Used for shader effects. *)
  offscreen_gl_id: int; (* OpenGL id for offscreen texture *)
  shader_prog: int option; (* program is an int *)
}

let format = Sdl.Pixel.format_rgba8888

let do_hide_cursor = false  (* It's buggy on WSL *)

let get_exn = function
  | Ok x -> x
  | Error(`Msg s) -> failwith s

let bigarray_create k len = Bigarray.(Array1.create k c_layout len)

let get_int =
  let a = bigarray_create Bigarray.int32 1 in
  fun f -> f a; Int32.to_int a.{0}

let set_int =
  let a = bigarray_create Bigarray.int32 1 in
  fun f i -> a.{0} <- Int32.of_int i; f a

let set_3d ba i x y z =
  let start = i * 3 in
  ba.{start} <- x; ba.{start + 1} <- y; ba.{start + 2} <- z

let vertices =
  let vs = bigarray_create Bigarray.float32 4 in
  set_3d vs 0 (-1.0) (-1.0) 0.;
  set_3d vs 0 (-1.0) 1.0 0.;
  set_3d vs 0 1.0 (-1.0) 0.;
  set_3d vs 0 1.0 1.0 0.;
  vs

let indices =
  let vs = bigarray_create Bigarray.int8_unsigned 6 in
  vs.{0} <- 0;
  vs.{1} <- 2;
  vs.{2} <- 1;

  vs.{3} <- 2;
  vs.{4} <- 3;
  vs.{5} <- 1;
  vs

let create_buffer b =
  let id = get_int (Gl.gen_buffers 1) in
  let bytes = Gl.bigarray_byte_size b in
  Gl.bind_buffer Gl.array_buffer id;
  Gl.buffer_data Gl.array_buffer bytes (Some b) Gl.static_draw;
  id

let create_geometry () =
  let gid = get_int (Gl.gen_vertex_arrays 1) in
  let iid = create_buffer indices in
  let vid = create_buffer vertices in
  let bind_attrib id loc dim typ =
    Gl.bind_buffer Gl.array_buffer id;
    Gl.enable_vertex_attrib_array loc;
    Gl.vertex_attrib_pointer loc dim typ false 0 (`Offset 0);
  in
  Gl.bind_vertex_array gid;
  Gl.bind_buffer Gl.element_array_buffer iid;
  bind_attrib vid 0 3 Gl.float;
  (* bind_attrib cid 1 3 Gl.float; *)
  Gl.bind_vertex_array 0;
  Gl.bind_buffer Gl.array_buffer 0;
  Gl.bind_buffer Gl.element_array_buffer 0

let clear_screen win =
  Sdl.render_clear win.renderer |> get_exn

let create w h ~zoom_x ~zoom_y ~shader_file =
  let out_w = Int.of_float @@ zoom_x *. Float.of_int w in
  let out_h = Int.of_float @@ zoom_y *. Float.of_int h in
  let window, renderer, shader_prog =
    Sdl.init Sdl.Init.video |> get_exn;
    let window = Sdl.create_window ~w:out_w ~h:out_h "Open Railroad Tycoon" Sdl.Window.opengl |> get_exn in
    let _ctx = Sdl.gl_create_context window |> get_exn in
    let renderer = Sdl.create_renderer window ~flags:Sdl.Renderer.accelerated |> get_exn in
    let shader_prog = Option.map Shader.load_crt_shader shader_file in
    window, renderer, shader_prog
  in
  let hide_cursor () =
    match Sdl.show_cursor false with
    | Ok x -> print_endline @@ Printf.sprintf "set show cursor: %b" x
    | _ -> print_endline "Failed to hide cursor"
  in
  if do_hide_cursor then hide_cursor ();
  Sdl.set_window_grab window true;
  Sdl.render_set_scale renderer zoom_x zoom_y |> get_exn;
  let rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0 in
  let rect2 = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0 in
  Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend |> get_exn;
  let offscreen_tex = Sdl.create_texture renderer format Sdl.Texture.access_target ~w ~h |> get_exn in 
  let offscreen_gl_id =
    let _ = Sdl.gl_bind_texture offscreen_tex |> get_exn in
    let gl_int = bigarray_create Bigarray.int32 1 in
    Tgl3.Gl.get_integerv Tgl3.Gl.texture_binding_2d gl_int;
    Sdl.gl_unbind_texture offscreen_tex |> get_exn;
    gl_int
  in
  {
    inner_w=w;
    inner_h=h;
    renderer;
    window;
    zoom_x;
    zoom_y;
    rect;
    rect2;
    opt_rect=Some rect;
    shader_prog;
    offscreen_tex=Some offscreen_tex;
    offscreen_gl_id;
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
  let r = win.renderer in
  let offscreen_tex = Sdl.create_texture r format Sdl.Texture.access_target ~w ~h |> get_exn |> Option.some in 
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
let render_to_texture win ~w ~h f =
  let old_tgt = Sdl.get_render_target win.renderer in
  Sdl.set_render_target win.renderer win.offscreen_tex |> get_exn;
  (* Sdl.set_render_draw_color t.renderer 0 0 0 255; *)
  (* Sdl.render_clear t.renderer; *)
  f ();  (* e.g., render_map (); render_trains (); *)
  Sdl.set_render_target win.renderer old_tgt |> get_exn

(* Modified render: Your main loop entry *)
let render_wrap win f =
  match win.shader_prog with
  | None ->
      (* No shader: Your original flow *)
      Sdl.render_clear win.renderer |> get_exn;
      (* Your render_texture calls *)
      f win;
      Sdl.render_present win.renderer |> get_exn;

  | Some state ->
      (* Helper: Fullscreen quad (immediate mode for simplicity; use VAO for perf) *)
      (* With shader *)
      let win_w, win_h = Sdl.get_window_size t.window in
      render_to_texture win ~w:win_w ~h:win_h (fun () ->
        Sdl.render_clear win.renderer;
        f win;
      );
      (* GL Post-Pass *)
      Tgl3.Gl.use_program win.shader_prog;
      Tgl3.Gl.active_texture Tgl3.Gl.texture0;
      Tgl3.Gl.bind_texture Tgl3.Gl.texture_2d win.offscreen_gl_id;
      (* Set uniforms, e.g., Tgl3.uniform1i (get_uniform_loc state "inputTexture") 0;
         Tgl3.uniform2f (get_uniform_loc state "resolution") (float win_w) (float win_h); *)
      let draw_fullscreen_quad () =
        Tgl3.Gl.begin_end `triangles @@ fun () ->
          (* Quad as two triangles; add tex coords if needed *)
          Tgl3.Gl.vertex2 (-1.0) (-1.0); Tgl3.vertex2 1.0 (-1.0); Tgl3.vertex2 (-1.0) 1.0;
          Tgl3.Gl.vertex2 1.0 (-1.0); Tgl3.vertex2 1.0 1.0; Tgl3.vertex2 (-1.0) 1.0;
      in
      draw_fullscreen_quad ();
      Sdl_gl.swap_window t.window

