open Tgl3

(* Helper: Load and compile shader source *)
let load_shader source shader_type =
  let open Tgl3.Gl in
  let shader = create_shader shader_type in
  shader_source shader "source";
  compile_shader shader;
  (* Check status *)
  let status = ref 0 in
  get_shaderiv shader compile_status status;
  if !status = 0 then begin
    let log_len = ref 0 in
    get_shaderiv shader info_log_length log_len;
    let log = String.make !log_len '\000' in
    get_shader_info_log shader !log_len log;
    failwith ("Shader compile error: " ^ log)
  end;
  shader

(* Create program from vert/frag sources *)
let create_program vert_src frag_src =
  let open Tgl3.Gl in
  let vert = load_shader vert_src `vert in
  let frag = load_shader frag_src `frag in
  let program = create_program () in
  attach_shader program vert;
  attach_shader program frag;
  link_program program;
  (* Check link status similarly *)
  let status = ref 0 in
  get_programiv program link_status status;
  if !status = 0 then failwith "Program link error";
  use_program program;
  program

(* Simple passthrough vertex shader *)
let simple_vert_src () = "#version 330 core\nin vec2 position; out vec2 vTexCoord; void main() { gl_Position = vec4(position, 0.0, 1.0); vTexCoord = (position + 1.0) * 0.5; }"

(* Load CRT shader from file *)
let load_crt_shader file_path =
  let frag_src = Core.In_channel.read_all file_path in
  let prog = create_program (simple_vert_src ()) frag_src in
  Some prog

