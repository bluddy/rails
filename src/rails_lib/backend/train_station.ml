open Containers
module Hashtbl = Utils.Hashtbl
module T = Train
module S = Station
module C = Constants

(* Functions concerning combination of trains and stations *)

let dump_unused_cars_to_station cars (stop:T.stop) station_supply =
  (* dump unused goods at the station at this stage *)
  (* return time for changing cars *)
  (* TODO: clear priority route cars *)
  match stop.consist_change with
  | None -> (* No adjustment *)
      0, 0, cars
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
      let expense = (added_cars - removed_cars) * C.car_cost in
      let work_done = removed_cars + added_cars in
      work_done, expense, train_cars
                           

let train_pickup_and_empty_station cars source cycle station_supply =
  (* Go over the train and find goods to fill it up with.
     Returns time for pickup and new cars
   *)
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
  | 0 -> 0, cars
  | _ ->
    let cars =
      List.map2 (fun (car:T.Car.t) add_amount ->
        match car.load with
        | Some load ->
          let amount = load.amount + add_amount in
          if load.amount < add_amount then
            let load = Some {T.Car.amount; source; cycle} in
            {car with load}
          else
            {car with load=Some {load with amount}}
        | None ->
            {car with load=Some {amount=add_amount; source; cycle}})
      cars
      pickup_amounts
    in
    let time_pickup =
      List.fold_left2 (fun time amt car ->
        let freight = T.Car.get_freight car |> Freight.to_enum in
        time + (amt * freight / 32))
        0
        pickup_amounts
        cars
    in
    let () =
      List.iter2 (fun car amount ->
        Hashtbl.decr station_supply (T.Car.get_good car) ~by:amount)
      cars
      pickup_amounts
    in
    time_pickup, cars

  (* Check whether a train stops at a particular size station *)
let train_class_stops_at station_info train = 
  let train = Train.train_type_to_enum train.Train.typ in
  let station = Station.kind_to_enum station_info.Station.kind in
  station > train

let train_stops_at (station:Station.t) train =
  (* Check if a train stops at a station for any reason *)
  match station.info with
  | Some station_info when train_class_stops_at station_info train -> true
  | Some _ when Utils.equal_loc (Train.get_dest train) (station.x, station.y) -> true
  | _ -> false

