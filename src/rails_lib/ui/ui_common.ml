open! Containers

module R = Renderer
module C = Constants

let draw_ui_car win ~x ~y ~full good =
  let freight = Freight.of_good good in
  let color = Freight.to_color freight ~full in
  let freight_idx = Freight.idx_of_good good in
  R.draw_rect win ~x ~y ~w:4 ~h:2 ~color ~fill:true;
  R.draw_point win ~x ~y:(y+2) ~color:Ega.black;
  R.draw_point win ~x:(x+3) ~y:(y+2) ~color:Ega.black;
  (* Draw background holes for certain classes *)
  match freight_idx with
  | 1 ->
    let y = y + 1 in
    R.draw_line win ~x1:(x+1) ~y1:y ~x2:(x+2) ~y2:y ~color:Ega.bblue
  | 2 ->
    R.draw_line win ~x1:(x+1) ~y1:y ~x2:(x+2) ~y2:y ~color:Ega.bblue
  | _ -> ()

