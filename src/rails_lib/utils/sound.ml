open Containers
open Tsdl
open Tsdl_mixer

module StringMap = Utils.StringMap

(* ------------------------- *)
(* Initialization *)
(* ------------------------- *)

let init () =
  Sdl.init Sdl.Init.audio |> ignore;
  Mixer.init [`WAV; `MP3; `OGG] |> ignore;
  Mixer.open_audio ~frequency:44100 ~format:Mixer.Fmt.s16_sys ~channels:2 ~chunksize:1024 |> ignore

(* ------------------------- *)
(* Music management *)
(* ------------------------- *)

type t= {
  music: Mixer.music option;
  sfx_table: Mixer.chunk StringMap.t;
}

let current_music = ref None

let play_music filename ~loops =
  (match !current_music with
   | Some m -> Mixer.halt_music (); Mixer.free_music m
   | None -> ());
  let music = Mixer.load_music filename in
  current_music := Some music;
  Mixer.play_music music ~loops

let stop_music () =
  Mixer.halt_music ();
  (match !current_music with
   | Some m -> Mixer.free_music m; current_music := None
   | None -> ())

(* ------------------------- *)
(* Sound effects management *)
(* ------------------------- *)

(* Table to store preloaded SFX *)
let sfx_table : (string, Mixer.chunk) Hashtbl.t = Hashtbl.create 32

(* Preload a sound effect *)
let preload_sfx name filename =
  let chunk = Mixer.load_wave filename in
  Hashtbl.replace sfx_table name chunk

(* Play a sound effect by name *)
let play_sfx ?(loops=0) name =
  match Hashtbl.find_opt sfx_table name with
  | Some chunk -> Mixer.play_channel ~channel:(-1) chunk ~loops |> ignore
  | None -> Printf.eprintf "Sound effect '%s' not found\n%!" name

(* Free all SFX *)
let free_all_sfx () =
  Hashtbl.iter (fun _ chunk -> Mixer.free_chunk chunk) sfx_table;
  Hashtbl.clear sfx_table

(* ------------------------- *)
(* Cleanup *)
(* ------------------------- *)

let cleanup () =
  stop_music ();
  free_all_sfx ();
  Mixer.close_audio ();
  Mixer.quit ()
