open Containers
open Utils.Infix

module R = Renderer

module AddCars = struct
  open Build_train_d

  let create_menu ?(suffix=[]) ~fonts region =
    let goods = Goods.of_region region in
    let car_list = List.map Goods.car_str_of goods in
    let goods_cars = List.combine car_list goods in
    let open Menu.MsgBox in
    make ~fonts ~heading:"Add Car?" ~x:200 ~y:10 @@
      [make_entry "No Thanks" (`Action `Done)] @
      (List.map (fun (name, good) -> make_entry name @@ `Action(`AddCar good)) goods_cars) @
      suffix

  (* Create the animation that will be used when we add cars *)
  let init (s:State.t) ~engine =
    (* Find station with engine shop *)
    let station =
      try
        Station_map.filter 
        (fun station -> Station.can_build_train station)
        s.backend.stations
        |> Iter.head_exn
      with Invalid_argument _ -> invalid_arg "No station with engine found"
    in
    let loc = station.x, station.y in
    let anim =
      let engine = engine.Engine.make in
      Train_animate_side.init s ~engine ~cars:[] ~paused:false ~station:loc ~rail:`Back
    in
    let menu = create_menu ~fonts:s.fonts s.backend.region in
    {
      anim;
      menu;
      show_menu=false;
      train_done=false;
    }

  let nobaction = Backend.Action.NoAction

  let handle_event (s:State.t) v event =
    if v.show_menu then
      let menu, action = Menu.MsgBox.update s v.menu event in
      let anim, show_menu, train_done =
        match action with
        | Menu.On(`AddCar good) ->
            let cars = v.anim.cars @ [good] in
            {v.anim with cars; paused=false}, false, false
        | Menu.On(`Done) ->
            (* Finished building train *)
            {v.anim with paused=false}, false, true
        | _ -> v.anim, true, false
      in
      let v =
        if menu =!= v.menu || anim =!= v.anim || show_menu =!= v.show_menu || train_done =!= v.train_done then
          {menu; anim; show_menu; train_done}
        else v
      in
      v, nobaction
    else if v.train_done && (Event.pressed_esc event || Event.is_left_click event) then
      let station = v.anim.station in
      let other_station =
        Track_graph.connected_stations_dirs s.backend.graph s.backend.track [station]
        |> Utils.LocuSet.to_iter |> Iter.head |> Option.map fst
      in
      v, Backend.Action.BuildTrain{engine=v.anim.engine;
                                   cars=v.anim.cars;
                                   station;
                                   other_station;
                                   player=0}
    else
      v, nobaction

  let handle_tick s v time =
    let anim = Train_animate_side.handle_tick s v.anim time in
    (* Check if we reached end *)
    let anim, v =
      if not v.show_menu && Train_animate_side.train_end_at_screen_edge s anim then (
        let menu = Menu.MsgBox.do_open_menu s v.menu ~selected:(Some 0) in
        Train_animate_side.pause anim, {v with show_menu = true; menu}
      ) else
        anim, v
    in
    if anim =!= v.anim then {v with anim} else v

  let render win (s:State.t) v =
    Train_animate_side.render win s v.anim;
    if v.show_menu then
      Menu.MsgBox.render win s v.menu;
    ()

  let is_done v = v.train_done

end

let nobaction = Backend.Action.NoAction

let handle_event (s:State.t) v (event:Event.t) = match v with
  | `ChooseEngine ->
      let engine_opt =
        Choose_engine.handle_event event s.backend.engines ~year:s.backend.year
      in
      begin match engine_opt with
      | Some engine ->
          let state = AddCars.init s ~engine in
          `AddCars state, nobaction
      | None ->
          `ChooseEngine, nobaction
      end
  | `AddCars state ->
      let state2, action = AddCars.handle_event s state event in
      if state =!= state2 then
        `AddCars state2, action
      else
        v, action

let render win (s:State.t) v = match v with
  | `ChooseEngine ->
        Choose_engine.render win s ~engines:s.backend.engines ~year:s.backend.year
  | `AddCars state ->
        AddCars.render win s state 

