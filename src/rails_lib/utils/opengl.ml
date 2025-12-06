open Containers
open Tgl3
open Result

type t = {
  pid: int;
  gid: int;
  loc_rubyTexture: int;
  loc_rubyInputSize: int;
  loc_rubyTextureSize: int;
}

type simple_progs = {
  texture_prog: int;
  color_prog: int;
}

let progs = ref None

let get_exn = function
  | Ok x -> x
  | Error(`Msg s) -> failwith s

let ( >>= ) x f = match x with Ok v -> f v | Error _ as e -> e

let bigarray_create k len = Bigarray.(Array1.create k c_layout len)

let get_int f =
  let a = bigarray_create Bigarray.int32 1 in
  f a; Int32.to_int a.{0}

let set_int f i =
  let a = bigarray_create Bigarray.int32 1 in
  a.{0} <- Int32.of_int i; f a

let get_string len f =
  let a = bigarray_create Bigarray.char len in
  f a; Gl.string_of_bigarray a

let set_3d ba i x y z =
  let start = i * 3 in
  ba.{start} <- x; ba.{start + 1} <- y; ba.{start + 2} <- z

let set_2d ba i x y =
  let start = i * 2 in
  ba.{start} <- x; ba.{start + 1} <- y

let compile_shader_ src typ =
  let get_shader sid e = get_int (Gl.get_shaderiv sid e) in
  let sid = Gl.create_shader typ in
  Gl.shader_source sid src;
  Gl.compile_shader sid;
  if get_shader sid Gl.compile_status = Gl.true_ then Ok sid else
  let len = get_shader sid Gl.info_log_length in
  let log = get_string len (Gl.get_shader_info_log sid len None) in
  (Gl.delete_shader sid; Error (`Msg log))

let create_program_ vertex_shader fragment_shader =
  let vid = compile_shader_ vertex_shader Gl.vertex_shader |> get_exn in
  let fid = compile_shader_ fragment_shader Gl.fragment_shader |> get_exn in
  let pid = Gl.create_program () in
  let get_program pid e = get_int (Gl.get_programiv pid e) in
  Gl.attach_shader pid vid; Gl.delete_shader vid;
  Gl.attach_shader pid fid; Gl.delete_shader fid;
  Gl.link_program pid;
  if get_program pid Gl.link_status = Gl.true_ then pid
  else (
    let len = get_program pid Gl.info_log_length in
    let log = get_string len @@ Gl.get_program_info_log pid len None in
    Gl.delete_program pid;
    failwith log
  )

(* Simple passthrough vertex shader *)
let simple_vert_src_ () = "#version 330 core\nlayout(location = 0) in vec2 a_position; out vec2 vTexCoord; void main() { gl_Position = vec4(a_position, 0.0, 1.0); vTexCoord = vec2(a_position.x + 1.0, 1.0 - a_position.y) / 2.0;}"
let simple_frag_src_textured () = "#version 330 core\n\
  uniform sampler2D tex;\n\
  uniform vec4 tex_coord_range;\n\
  uniform vec4 tex_sub_region;\n\
  in vec2 vTexCoord;\n\
  out vec4 color;\n\
  void main() {\n\
    float u = (vTexCoord.x - tex_coord_range.x) / (tex_coord_range.z - tex_coord_range.x);\n\
    float v = (vTexCoord.y - tex_coord_range.y) / (tex_coord_range.w - tex_coord_range.y);\n\
    vec2 real_coord = vec2(tex_sub_region.x + u * tex_sub_region.z, tex_sub_region.y + v * tex_sub_region.w);\n\
    color = texture(tex, real_coord);\n\
  }"
let simple_frag_src_colored () = "#version 330 core\nuniform vec4 u_color; out vec4 color; void main() { color = u_color; }"

let init () =
  let textured_prog = create_program_ (simple_vert_src_()) (simple_frag_src_textured()) in
  let colored_prog = create_program_ (simple_vert_src_()) (simple_frag_src_colored()) in
  progs := Some { texture_prog = textured_prog; color_prog = colored_prog }

let get_progs () = match !progs with
  | Some p -> p
  | None -> failwith "Opengl.init not called"

(* Create a normal RGBA8 texture (for game frame and offscreen) *)
let create_texture w h =
  let tex = get_int (Gl.gen_textures 1) in
  Gl.bind_texture Gl.texture_2d tex;
  Gl.tex_image2d Gl.texture_2d 0 Gl.rgba8 w h 0 Gl.rgba Gl.unsigned_byte (`Offset 0);
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.nearest;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.nearest;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_s Gl.clamp_to_edge;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_t Gl.clamp_to_edge;
  tex

(* Create a streaming texture that you can read/write CPU-side *)
let create_streaming_texture w h =
  let tex = create_texture w h in
  (* No special flags needed — we just use glTexSubImage2D *)
  tex

(* Read pixels from any GL texture (used by Transition) *)
let read_texture_pixels tex w h =
  let pixels = Bigarray.(Array1.create int32 c_layout (w * h)) in
  Gl.bind_texture Gl.texture_2d tex;
  Gl.get_tex_image Gl.texture_2d 0 Gl.rgba Gl.unsigned_int_8_8_8_8 pixels;
  pixels

(* Upload pixels to any GL texture *)
let upload_texture tex w h pixels =
  Gl.bind_texture Gl.texture_2d tex;
  Gl.tex_sub_image2d Gl.texture_2d 0 0 0 w h Gl.rgba Gl.unsigned_int_8_8_8_8 (`Data pixels)

let white_texture =
  let pixel = Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout [| 0xffffffffl |] in
  let tex = get_int (Gl.gen_textures 1) in
  Gl.bind_texture Gl.texture_2d tex;
  Gl.tex_image2d Gl.texture_2d 0 Gl.rgba8 1 1 0 Gl.rgba Gl.unsigned_int_8_8_8_8 (`Data pixel);
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.nearest;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.nearest;
  tex

let create_fbo tex =
  let fbo = get_int (Gl.gen_framebuffers 1) in
  Gl.bind_framebuffer Gl.framebuffer fbo;
  Gl.framebuffer_texture2d Gl.framebuffer Gl.color_attachment0 Gl.texture_2d tex 0;
  let status = Gl.check_framebuffer_status Gl.framebuffer in
  if status <> Gl.framebuffer_complete then (
    let err =
      if status = Gl.framebuffer_undefined then "framebuffer_undefined"
      else if status = Gl.framebuffer_incomplete_attachment then "framebuffer_incomplete_attachment"
      else if status = Gl.framebuffer_incomplete_missing_attachment then "framebuffer_incomplete_missing_attachment"
      else if status = Gl.framebuffer_incomplete_draw_buffer then "framebuffer_incomplete_draw_buffer"
      else if status = Gl.framebuffer_incomplete_read_buffer then "framebuffer_incomplete_read_buffer"
      else if status = Gl.framebuffer_unsupported then "framebuffer_unsupported"
      else if status = Gl.framebuffer_incomplete_multisample then "framebuffer_incomplete_multisample"
      else if status = Gl.framebuffer_incomplete_layer_targets then "framebuffer_incomplete_layer_targets"
      else "unknown framebuffer error"
    in
    failwith ("Framebuffer not complete: " ^ err)
  );
  Gl.bind_framebuffer Gl.framebuffer 0;
  fbo

let draw_colored_quad x y w h r g b a ~inner_w ~inner_h =
  let p = get_progs () in
  Gl.use_program p.color_prog;

  let x1 = (float x /. float inner_w  *. 2.0)  -. 1.0 in
  let y1 = 1.0 -. (float y /. float inner_h  *. 2.0) in
  let x2 = (float (x+w) /. float inner_w *. 2.0) -. 1.0 in
  let y2 = 1.0 -. (float (y+h) /. float inner_h *. 2.0) in

  let verts = bigarray_create Bigarray.float32 (4 * 2) in
  set_2d verts 0 x1 y2;   (* top-left     *)
  set_2d verts 1 x2 y2;   (* top-right    *)
  set_2d verts 2 x1 y1;   (* bottom-left  *)
  set_2d verts 3 x2 y1;   (* bottom-right *)

  let loc = Gl.get_uniform_location p.color_prog "u_color" in
  Gl.uniform4f loc r g b a;
  Gl.buffer_data Gl.array_buffer (Gl.bigarray_byte_size verts) (Some verts) Gl.stream_draw;
  Gl.draw_arrays Gl.triangle_strip 0 4

let draw_rect ~inner_w ~inner_h ~x ~y ~w ~h ~color:(r,g,b,a) ~fill:_ =
  let r,g,b,a = float r /. 255., float g /. 255., float b /. 255., float a /. 255. in
  draw_colored_quad x y w h r g b a ~inner_w ~inner_h

let draw_point ~inner_w ~inner_h ~x ~y =
  draw_rect ~inner_w ~inner_h ~x ~y ~w:1 ~h:1 ~color:(255,255,255,255) ~fill:true

let vertices =
  let vs = bigarray_create Bigarray.float32 (4 * 2) in
  set_2d vs 0 (-1.0) (-1.0);
  set_2d vs 1 1.0 (-1.0);
  set_2d vs 2 (-1.0) 1.0;
  set_2d vs 3 1.0 1.0;
  vs

let draw_textured_quad_sub tex_id ~from_x ~from_y ~from_w ~from_h ~to_x ~to_y ~to_w ~to_h ~tex_w ~tex_h ~inner_w ~inner_h =
  let p = get_progs () in
  Gl.use_program p.texture_prog;

  let x1 = (float to_x /. float inner_w  *. 2.0)  -. 1.0 in
  let y1 = 1.0 -. (float to_y /. float inner_h  *. 2.0) in
  let x2 = (float (to_x+to_w) /. float inner_w *. 2.0) -. 1.0 in
  let y2 = 1.0 -. (float (to_y+to_h) /. float inner_h *. 2.0) in

  let verts = bigarray_create Bigarray.float32 (4 * 2) in
  set_2d verts 0 x1 y2;   (* top-left     *)
  set_2d verts 1 x2 y2;   (* top-right    *)
  set_2d verts 2 x1 y1;   (* bottom-left  *)
  set_2d verts 3 x2 y1;   (* bottom-right *)

  Gl.active_texture Gl.texture0;
  Gl.bind_texture Gl.texture_2d tex_id;
  let loc = Gl.get_uniform_location p.texture_prog "tex" in
  Gl.uniform1i loc 0;

  let u1 = (x1 +. 1.0) /. 2.0 in
  let v1 = (1.0 -. y1) /. 2.0 in
  let u2 = (x2 +. 1.0) /. 2.0 in
  let v2 = (1.0 -. y2) /. 2.0 in
  let range_loc = Gl.get_uniform_location p.texture_prog "tex_coord_range" in
  Gl.uniform4f range_loc u1 v2 u2 v1; (* min_u, min_v, max_u, max_v *)

  let sub_x = float from_x /. float tex_w in
  let sub_y = float from_y /. float tex_h in
  let sub_w = float from_w /. float tex_w in
  let sub_h = float from_h /. float tex_h in
  let sub_region_loc = Gl.get_uniform_location p.texture_prog "tex_sub_region" in
  Gl.uniform4f sub_region_loc sub_x sub_y sub_w sub_h;

  Gl.buffer_data Gl.array_buffer (Gl.bigarray_byte_size verts) (Some verts) Gl.stream_draw;
  Gl.draw_arrays Gl.triangle_strip 0 4

let draw_textured_quad tex_id ~x ~y ~w ~h ~tex_w ~tex_h ~inner_w ~inner_h =
  draw_textured_quad_sub tex_id ~from_x:0 ~from_y:0 ~from_w:tex_w ~from_h:tex_h
    ~to_x:x ~to_y:y ~to_w:w ~to_h:h ~tex_w ~tex_h ~inner_w ~inner_h

let create_buffer_ b =
  let id = get_int (Gl.gen_buffers 1) in
  let bytes = Gl.bigarray_byte_size b in
  Gl.bind_buffer Gl.array_buffer id;
  Gl.buffer_data Gl.array_buffer bytes (Some b) Gl.static_draw;
  id

let create_geometry_ () =
  (* Create geometry for a simple quad *)
  let gid = get_int (Gl.gen_vertex_arrays 1) in
  let vid = create_buffer_ vertices in
  let bind_attrib id loc dim typ =
    Gl.bind_buffer Gl.array_buffer id;
    Gl.enable_vertex_attrib_array loc;
    Gl.vertex_attrib_pointer loc dim typ false 0 (`Offset 0);
  in
  Gl.bind_vertex_array gid;
  bind_attrib vid 0 2 Gl.float;

  (* Clean up *)
  Gl.bind_vertex_array 0;
  Gl.bind_buffer Gl.array_buffer 0;
  Gl.bind_buffer Gl.element_array_buffer 0;
  gid

let delete_program pid =
  Gl.delete_program pid; Ok ()

(* Define the required preprocessor directives *)
let vertex_define = "#define VERTEX\n"
let fragment_define = "#define FRAGMENT\n"

let insert_define_after_version define_str raw_src =
  (* 1. Find the position of the first newline character ('\n'). *)
  match String.index_opt raw_src '\n' with
  | Some index ->
    (* 2. Split the string into the part before the newline (the #version line)
          and the part after the newline (the rest of the code). *)
    let version_line = String.sub raw_src 0 (index + 1) in
    let rest_of_code = String.sub raw_src (index + 1) (String.length raw_src - index - 1) in
    (* 3. Reconstruct the string: [Version Line] + [Define] + [Rest of Code] *)
    version_line ^ define_str ^ rest_of_code

  | None ->
    (* Handle the case where the file is only the version line, though unlikely. *)
    failwith "Shader source does not contain a newline after #version"

let create shader_path =
  let gid = create_geometry_ () in
  let pid =
    let raw_src = IO.with_in shader_path IO.read_all in
    let vert_src = insert_define_after_version vertex_define raw_src in
    let frag_src = insert_define_after_version fragment_define raw_src in
    create_program_ vert_src frag_src
  in
  let get_loc name = Gl.get_uniform_location pid name in
  {
    pid; gid;
    loc_rubyTexture = get_loc "rubyTexture";
    loc_rubyInputSize = get_loc "rubyInputSize";
    loc_rubyTextureSize = get_loc "rubyTextureSize";
  }

let gl_id_of_sdl_tex tex =
  (* Convert an SDL texture to the GL id *)
  let _ = Tsdl.Sdl.gl_bind_texture tex |> get_exn in
  let gl_int = get_int @@ Tgl3.Gl.get_integerv Tgl3.Gl.texture_binding_2d in
  Tsdl.Sdl.gl_unbind_texture tex |> get_exn;
  gl_int

let draw_quad_with_tex v tex_id win ~inner_w ~inner_h =
  Gl.bind_framebuffer Gl.framebuffer 0;
  let win_w, win_h = Tsdl.Sdl.get_window_size win in

  Gl.viewport 0 0 win_w win_h;
  Gl.clear_color 0. 0. 0. 1.;
  Gl.clear Gl.color_buffer_bit;

  (* shader *)
  Gl.use_program v.pid;

  (* Texture *)
  Gl.active_texture Gl.texture0;
  Gl.bind_texture Gl.texture_2d tex_id;
  Gl.uniform1i v.loc_rubyTexture 0;

  (* Resolution uniforms *)
  let input_w = float inner_w in
  let input_h = float inner_h in
  let tex_w, tex_h = input_w, input_h in (* since offscreen tex is inner size *)

  Gl.uniform2f v.loc_rubyInputSize input_w input_h;
  Gl.uniform2f v.loc_rubyTextureSize tex_w tex_h;

  Gl.bind_vertex_array v.gid; (* vertices *)
  Gl.draw_arrays Gl.triangle_strip 0 4;

  Tsdl.Sdl.gl_swap_window win

