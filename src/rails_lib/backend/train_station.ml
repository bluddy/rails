open Containers
module Hashtbl = Utils.Hashtbl
module T = Train
module S = Station
module C = Constants

(* Functions concerning combination of trains and stations *)

let dump_unused_cars_to_station cars (stop:T.stop) station_supply =
  (* dump unused goods at the station at this stage *)
  (* return time for changing cars *)
  match stop.consist_change with
  | None -> (* No adjustment *)
      0, Money.zero, cars
  | Some stop_cars ->
      let train_cars_by_good = 
        let h = Hashtbl.create 10 in
        List.iter (fun car -> Hashtbl.add_list h (T.Car.get_good car) car)
        cars;
        h
      in
      (* Create the new cars for the train using old cars if possible *)
      let added_cars, train_cars =
        List.fold_map (fun cnt good ->
          match Hashtbl.get train_cars_by_good good with
          | Some (car::cars) ->
              Hashtbl.replace train_cars_by_good good cars;
              cnt, car
          | _ ->
              let car = T.Car.make good in
              cnt + 1, car)
        0
        stop_cars
      in
      (* Remaining cars are added to the station supply *)
      let removed_cars =
        Hashtbl.fold (fun good cars cnt ->
          List.fold_left (fun cnt car ->
            Hashtbl.incr ~by:(T.Car.get_amount car) station_supply good;
            cnt + 1)
          cnt
          cars)
        train_cars_by_good
        0
      in
      let expense = Money.mult C.car_cost (added_cars - removed_cars) in
      let work_done = removed_cars + added_cars in
      work_done, expense, train_cars
                           

let train_pickup_and_empty_station cars loc cycle station =
  (* Go over the train and find goods to fill it up with.
     Returns time for pickup and new cars
   *)
  let station_supply = Station.get_supply_exn station in
  let total_pickup, pickup_amounts = 
    List.fold_map (fun total car ->
      let car_amount, good = T.Car.get_amount car, T.Car.get_good car in
      let station_amount_of_good = Hashtbl.get_or station_supply good ~default:0 in
      let amount_to_take =
        Utils.clip station_amount_of_good ~min:0 ~max:(C.car_amount - car_amount)
      in
      total + amount_to_take, amount_to_take)
    0
    cars
  in
  (* Shortcut if we can pick up nothing *)
  match total_pickup with
  | 0 -> 0, cars, station
  | _ ->
    let cars_pickup_amounts = List.combine cars pickup_amounts in
    let cars =
      List.map (fun (car, add_amount) ->
        match car.T.Car.load with
        | Some load ->
          let amount = load.amount + add_amount in
          if load.amount < add_amount then
            let load = Some {T.Car.amount; source=loc; cycle} in
            {car with load}
          else
            {car with load=Some {load with amount}}
        | None ->
            {car with load=Some {amount=add_amount; source=loc; cycle}}
      )
      cars_pickup_amounts
    in
    let time_pickup =
      List.fold_left (fun time (car, amt) ->
        let freight = T.Car.get_freight car |> Freight.to_enum in
        time + (amt * freight / 32))
        0
        cars_pickup_amounts
    in
    let station_picked_up_goods = Station.get_picked_up_goods_exn station in
    let () =
      List.iter (fun (car, amount) ->
        let good = T.Car.get_good car in
        Hashtbl.decr station_supply good ~by:amount;
        Hashtbl.incr station_picked_up_goods good ~by:amount;
      )
      cars_pickup_amounts
    in
    time_pickup, cars, station

  (* Check whether a train stops at a particular size station *)
let train_class_stops_at station_info train = 
  let train = Train.train_type_to_enum train.Train.typ in
  let station = Station.kind_to_enum station_info.Station.kind in
  station > train

let train_stops_at (station:Station.t) train =
  (* Check if a train stops at a station for any reason *)
  match station.info with
  | Some station_info when train_class_stops_at station_info train -> true
  | Some _ when Utils.equal_loc (Train.get_dest train) @@ Station.get_loc station -> true
  | _ -> false

let remove_train train_idx blocks trainmap =
  (* Same function called by Backend *)
  let () =
    let train = Trainmap.get train_idx trainmap in
    match train.state with
    | Traveling {block; _} -> Block_map.block_decr_train block blocks
    | _ -> ()
  in
  Trainmap.delete train_idx trainmap


