open! Containers

module R = Renderer
module C = Constants

let draw_ui_car win ~x ~y ~full good =
  let freight = Goods.freight_of_goods good in
  let color = Goods.color_of_freight freight ~full in
  R.draw_rect win ~x ~y ~w:4 ~h:2 ~color ~fill:true;
  R.draw_point win ~x ~y:(y+2) ~color:Ega.black;
  R.draw_point win ~x:(x+3) ~y:(y+2) ~color:Ega.black

