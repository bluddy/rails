open Containers
open Tsdl
module Ndarray = Owl_base_dense_ndarray.Generic

type opengl_state = {
  fbo: int;
  framebuffer_tex: int;
  transition_tex: int;
}

type window = {
  zoom_x: float;
  zoom_y: float;
  inner_w: int;
  inner_h: int;
  window: Sdl.window;
  opengl: opengl_state;
  shader_prog: Opengl.t option; (* program is an int *)
}

let format = Sdl.Pixel.format_rgba8888

let do_hide_cursor = false  (* It's buggy on WSL *)

let get_exn = function
  | Ok x -> x
  | Error(`Msg s) -> failwith s

let clear_screen (_win:window) =
  Tgl3.Gl.clear_color 0. 0. 0. 1.;
  Tgl3.Gl.clear Tgl3.Gl.color_buffer_bit

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

  Opengl.init ();

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

  let framebuffer_tex = Opengl.create_texture w h in
  let transition_tex  = Opengl.create_streaming_texture w h in
  let fbo = Opengl.create_fbo framebuffer_tex in
  let opengl = { fbo; framebuffer_tex; transition_tex } in

  {
    inner_w=w;
    inner_h=h;
    window;
    zoom_x;
    zoom_y;
    opengl;
    shader_prog;
  }

let zoom _win x = x
  (* win.zoom *. Float.of_int x |> Int.of_float *)

let height window = window.inner_h
let width window = window.inner_w

module Transition = struct

type t = {
  w: int; h: int;
  offscreen_tex_gl: int;  (* Used for transition effects. *)
  mutable pixels: (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t; (* Copy from render to do transition *)
  tex_gl: int; (* transition texture *)
  mutable offsets: int list; (* offsets into screen *)
}

let make win random =
  let w, h = win.inner_w, win.inner_h in
  let offscreen_tex_gl = Opengl.create_streaming_texture w h in
  let pixels = Bigarray.Array1.(create Bigarray.int32 Bigarray.c_layout (h*w)) in
  let tex_gl = Opengl.create_streaming_texture w h in
  let offsets = Iter.(0 -- (h * w - 1)) |> Iter.to_array in
  Array.shuffle_with random offsets;
  let offsets = Array.to_list offsets in
  {w; h; offscreen_tex_gl; pixels; tex_gl; offsets}

let copy_pixels_to_tex v =
  Opengl.upload_texture v.tex_gl v.w v.h v.pixels

let render_offscreen win old_render_fn render_fn v =
  (* Do once with final transition image. Render offscreen the next image to our texture. *)
  let fbo = Opengl.create_fbo v.offscreen_tex_gl in
  Tgl3.Gl.bind_framebuffer Tgl3.Gl.framebuffer fbo;
  Tgl3.Gl.viewport 0 0 v.w v.h;

  (* Old image *)
  old_render_fn win;
  (* Read from texture target to a buffer we can read from *)
  let old_pixels = Opengl.read_texture_pixels_flipped v.offscreen_tex_gl v.w v.h in
  
  (* New image *)
  render_fn win;
  (* Read from texture target to a buffer we can read from *)
  v.pixels <- Opengl.read_texture_pixels_flipped v.offscreen_tex_gl v.w v.h;

  (* upload old pixels to the transition texture to start *)
  Opengl.upload_texture v.tex_gl v.w v.h old_pixels;

  (* Restore render target to the main screen *)
  Tgl3.Gl.bind_framebuffer Tgl3.Gl.framebuffer 0;
  Tgl3.Gl.delete_framebuffers 1 (Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout [| Int32.of_int fbo |])


let step num_pixels v =
  let current_pixels = Opengl.read_texture_pixels v.tex_gl v.w v.h in
  let rec loop n =
    if n = 0 then `NotDone else
    match v.offsets with
    | [] -> `Done
    | i::_is ->
      v.offsets <- _is;
      let new_pixel = Bigarray.Array1.get v.pixels i in
      Bigarray.Array1.set current_pixels i new_pixel;
      loop (n - 1)
  in
  let res = loop num_pixels in
  Opengl.upload_texture v.tex_gl v.w v.h current_pixels;
  res

let render win v =
    Opengl.draw_textured_quad v.tex_gl ~x:0 ~y:0 ~w:v.w ~h:v.h ~inner_w:win.inner_w ~inner_h:win.inner_h

end

module Texture = struct
  type t = {
    h: int;
    w: int;
    texture: int;
  }

  let get_w t = t.w
  let get_h t = t.h

  let make _win (arr:Pic.ndarray) =
    let h, w = Ndarray.nth_dim arr 0, Ndarray.nth_dim arr 1 in
    let ndarray_u8 = Bigarray.reshape_1 arr (w*h*4) in
    let ndarray_i32 = Bigarray.(Array1.create int32 c_layout (w*h)) in
    for i = 0 to w*h-1 do
      let r = Bigarray.Array1.get ndarray_u8 (i*4+0) in
      let g = Bigarray.Array1.get ndarray_u8 (i*4+1) in
      let b = Bigarray.Array1.get ndarray_u8 (i*4+2) in
      let a = Bigarray.Array1.get ndarray_u8 (i*4+3) in
      let i32 = Int32.of_int ((a lsl 24) lor (b lsl 16) lor (g lsl 8) lor r) in
      Bigarray.Array1.set ndarray_i32 i i32;
    done;

    let texture = Opengl.create_streaming_texture w h in
    Opengl.upload_texture texture w h ndarray_i32;
    
    Tgl3.Gl.enable Tgl3.Gl.blend;
    Tgl3.Gl.blend_func Tgl3.Gl.src_alpha Tgl3.Gl.one_minus_src_alpha;

    { w; h; texture }

  let destroy tex =
    Tgl3.Gl.delete_textures 1 (Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout [| Int32.of_int tex.texture |])

    (* slowish *)
  let update (tex:t) (ndarray:Pic.ndarray) =
    let h, w = Ndarray.nth_dim ndarray 0, Ndarray.nth_dim ndarray 1 in
    let ndarray_u8 = Bigarray.reshape_1 ndarray (w*h*4) in
    let ndarray_i32 = Bigarray.(Array1.create int32 c_layout (w*h)) in
    for i = 0 to w*h-1 do
      let r = Bigarray.Array1.get ndarray_u8 (i*4+0) in
      let g = Bigarray.Array1.get ndarray_u8 (i*4+1) in
      let b = Bigarray.Array1.get ndarray_u8 (i*4+2) in
      let a = Bigarray.Array1.get ndarray_u8 (i*4+3) in
      let i32 = Int32.of_int ((a lsl 24) lor (b lsl 16) lor (g lsl 8) lor r) in
      Bigarray.Array1.set ndarray_i32 i i32;
    done;
    Opengl.upload_texture tex.texture tex.w tex.h ndarray_i32

  let render ?color ~x ~y win tex =
    let color = Option.value color ~default:(255,255,255,255) in
    Opengl.draw_textured_quad ~color tex.texture ~x ~y ~w:tex.w ~h:tex.h ~inner_w:win.inner_w ~inner_h:win.inner_h

    (* Render only a part of the texture *)
    (* Use different rects so we don't disturb the texture's w and h *)
  let render_subtex ?w ?h ?(from_x=0) ?(from_y=0) ~x ~y win tex =
    let w = match w with | None -> get_w tex | Some w -> w in
    let h = match h with | None -> get_h tex | Some h -> h in
    Opengl.draw_textured_quad_sub tex.texture
      ~from_x ~from_y ~from_w:w ~from_h:h
      ~to_x:x ~to_y:y ~to_w:w ~to_h:h
      ~tex_w:tex.w ~tex_h:tex.h
      ~inner_w:win.inner_w ~inner_h:win.inner_h
end

let _set_color _win _color =
  () (* Colors are passed to draw_rect now *)

let draw_rect win ~x ~y ~w ~h ~color ~fill =
  Opengl.draw_rect ~inner_w:win.inner_w ~inner_h:win.inner_h ~x ~y ~w ~h ~color ~fill

let paint_screen win ~color =
  draw_rect win ~x:0 ~y:0 ~w:(width win) ~h:(height win) ~color ~fill:true

let draw_point ?color win ~x ~y =
  let color = Option.value color ~default:(255,255,255,255) in
  Opengl.draw_rect ~inner_w:win.inner_w ~inner_h:win.inner_h ~x ~y ~w:1 ~h:1 ~color ~fill:true

  (* Bresenham's algorithm *)
let draw_line win ~x1 ~y1 ~x2 ~y2 ~color =
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
        draw_point ~color win ~x ~y;
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
        draw_point ~color win ~x ~y;
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
  Tgl3.Gl.bind_framebuffer Tgl3.Gl.framebuffer win.opengl.fbo;
  Tgl3.Gl.viewport 0 0 win.inner_w win.inner_h;
  f ();
  Tgl3.Gl.bind_framebuffer Tgl3.Gl.framebuffer 0

(* Modified render: Your main loop entry *)
let render_wrap win f x =
  (* Always render to offscreen FBO then display through shader *)
  render_to_texture win (fun () ->
    clear_screen win;
    f x;
  );
  match win.shader_prog with
  | Some state ->
      (* Display with custom shader effects *)
      Opengl.draw_quad_with_tex state win.opengl.framebuffer_tex win.window ~inner_w:win.inner_w ~inner_h:win.inner_h
  | None ->
      (* Display with simple scaling (no shader effects) *)
      Tgl3.Gl.bind_framebuffer Tgl3.Gl.framebuffer 0;
      let win_w, win_h = Tsdl.Sdl.get_window_size win.window in
      Tgl3.Gl.viewport 0 0 win_w win_h;
      Opengl.draw_textured_quad win.opengl.framebuffer_tex ~x:0 ~y:0 ~w:win.inner_w ~h:win.inner_h ~inner_w:win.inner_w ~inner_h:win.inner_h;
      Sdl.gl_swap_window win.window

