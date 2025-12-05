open Containers
open Tgl3
open Result

type t = {
  pid: int;
  gid: int;
}

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

let create_buffer_ b =
  let id = get_int (Gl.gen_buffers 1) in
  let bytes = Gl.bigarray_byte_size b in
  Gl.bind_buffer Gl.array_buffer id;
  Gl.buffer_data Gl.array_buffer bytes (Some b) Gl.static_draw;
  id

let create_geometry_ () =
  (* Create geometry for a simple quad *)
  let gid = get_int (Gl.gen_vertex_arrays 1) in
  let iid = create_buffer_ indices in
  let vid = create_buffer_ vertices in
  let bind_attrib id loc dim typ =
    Gl.bind_buffer Gl.array_buffer id;
    Gl.enable_vertex_attrib_array loc;
    Gl.vertex_attrib_pointer loc dim typ false 0 (`Offset 0);
  in
  Gl.bind_vertex_array gid;
  Gl.bind_buffer Gl.element_array_buffer iid;
  bind_attrib vid 0 3 Gl.float;

  (* Clean up *)
  Gl.bind_vertex_array 0;
  Gl.bind_buffer Gl.array_buffer 0;
  Gl.bind_buffer Gl.element_array_buffer 0;
  gid

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
  Gl.bind_attrib_location pid 0 "vertex";
  Gl.bind_attrib_location pid 1 "color";
  Gl.link_program pid;
  if get_program pid Gl.link_status = Gl.true_ then pid
  else (
    let len = get_program pid Gl.info_log_length in
    let log = get_string len @@ Gl.get_program_info_log pid len None in
    Gl.delete_program pid;
    failwith log
  )

let delete_program pid =
  Gl.delete_program pid; Ok ()

(* Simple passthrough vertex shader *)
let simple_vert_src_ () = "#version 330 core\nin vec2 position; out vec2 vTexCoord; void main() { gl_Position = vec4(position, 0.0, 1.0); vTexCoord = (position + 1.0) * 0.5; }"

let create shader_path =
  let gid = create_geometry_ () in
  let pid =
    let frag_src = IO.with_in shader_path IO.read_all in
    create_program_ (simple_vert_src_ ()) frag_src
  in
  {pid; gid}

let gl_id_of_sdl_tex tex =
  (* Convert an SDL texture to the GL id *)
  let _ = Tsdl.Sdl.gl_bind_texture tex |> get_exn in
  let gl_int = get_int @@ Tgl3.Gl.get_integerv Tgl3.Gl.texture_binding_2d in
  Tsdl.Sdl.gl_unbind_texture tex |> get_exn;
  gl_int

let draw_quad_with_tex v tex win =
  Gl.clear_color 0. 0. 0. 1.;
  Gl.clear Gl.color_buffer_bit;
  Gl.use_program v.pid; (* shader *)
  (* Texture *)
  Gl.active_texture Gl.texture0;
  Gl.bind_texture Gl.texture_2d tex;
  Gl.bind_vertex_array v.gid; (* vertices *)
  Gl.draw_elements Gl.triangles 6 Gl.unsigned_byte (`Offset 0);
  Gl.bind_vertex_array 0; (* unbind *)
  Tsdl.Sdl.gl_swap_window win

