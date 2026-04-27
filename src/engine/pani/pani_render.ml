open Containers

module C = Pani_const
module List = Utils.List
module R = Renderer

type status = [`Pause | `Done | `Init ]

let sp = Printf.sprintf

type sound_player = {
  play_music: unit -> unit;
  stop_music: unit -> unit;
}

type t = {
  mutable status: status;
  interp: Pani_interp.t;
  mutable last_time: int;
  mutable textures: R.Texture.t option array;
  mutable bg_tex: R.Texture.t option;
  sound: sound_player option;
}

let create ?(dump=false) ?debug ?input ?sound filename =
  let stream = Utils.stream_of_file filename in
  let interp = Pani.of_stream ~dump ?debug ?input stream in
  let textures = [||] in
  let status = `Init in
  let last_time = 0 in
  {status; interp; last_time; textures; bg_tex=None; sound}

let render ?(clear_screen=true) win v =
  (* Get around not having access to win elsewhere *)
  let no_textures = Array.length v.textures = 0 in
  if no_textures then (
    let textures = Array.map (Option.map (R.Texture.make win)) v.interp.pics in
    v.textures <- textures;
    Option.iter (fun bg -> v.bg_tex <- R.Texture.make win bg |> Option.some) v.interp.background
  );

  if clear_screen then R.clear_screen win;

  Option.iter (fun bg_tex -> R.Texture.render win ~x:0 ~y:0 bg_tex) v.bg_tex;

  List.rev_iter (fun Pani_interp.{pic_idx; x; y} ->
    (* Note: why does 0 turn up here and doesn't exist? *)
    if pic_idx <> -1 then
    let tex = v.textures.(pic_idx) |> Option.get_exn_or @@ sp "missing texture %d" pic_idx in
    R.Texture.render win ~x ~y tex
  ) v.interp.static_pics;

  Iter.iter (fun i ->
    let sprite = Pani_interp.anim_get_pic v.interp i in
    if sprite.active && not sprite.background && sprite.pic_idx <> -1 then (
      let tex = v.textures.(sprite.pic_idx) |> Option.get_exn_or @@ sp "missing texture %d" sprite.pic_idx in
      let x, y = Pani_interp.calc_anim_xy v.interp i in
      R.Texture.render win ~x ~y tex;

      match v.interp.debugger with
      | Some debugger ->
          (match debugger.cur_sprite with
          | `Some cur_sprite when i = cur_sprite ->
            R.draw_rect win ~x ~y ~w:tex.w ~h:tex.h ~color:Ega.bred ~fill:false
          | _ -> ())
      | _ -> ()
    )
  )
  Iter.(0 -- Pani_const.max_num_sprites)

let handle_tick time v =
  let v = match v.status with
    | `Init ->
        Option.iter (fun sound -> sound.play_music ()) v.sound;
        {v with status=`Pause}
    | `Pause ->
        if time - v.last_time > Pani_const.update_delta
        then (
          v.last_time <- time;
          v.status <- Pani_interp.step v.interp;
        );
        v
    | `Done ->
        Option.iter (fun sound -> sound.stop_music ()) v.sound;
        v
  in
  v, `Stay

let handle_event event v =
  if Event.modal_dismiss event then (
    Option.iter (fun sound -> sound.stop_music ()) v.sound;
    {v with status=`Done}, `Exit
  ) else
    v, `Stay

let standalone win ~filename =
  let handle_event v _event _time = v, `Stay in
  let v = create filename in
  let funcs = Mainloop.{
    handle_tick=(fun v time -> handle_tick time v);
    render=Renderer.render_wrap win (render win);
    handle_event;
  }
  in
  v, funcs

let debugger ?(dump=false) win ~filename =
  Pani_interp.set_debug true;
  Pani_sprite.set_debug true;
  let handle_event v event _time = match event with
    | Event.Key {key=Event.N; down=true; _} ->
        let _ = Pani_interp.debugger_step v.interp in
        v, `Stay
    | Event.Key {key=Event.S; down=true; _} ->
        let _ = Pani_interp.debugger_step_sprite v.interp in
        v, `Stay
    | Event.Key {key=Event.Q; down=true; _} ->
        v, `Exit
    | _ ->
        v, `Stay
  in
  let v = create ~dump ~debug:true filename in
  (* Do one step to set up all the animations *)
  let _ = Pani_interp.step v.interp in
  let funcs = Mainloop.{
    handle_tick=(fun v _ -> v, `Stay);
    render=render win;
    handle_event;
  }
  in
  v, funcs

