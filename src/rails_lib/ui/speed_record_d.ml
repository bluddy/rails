
type loc = Utils.loc

type t = {
  speed: int;
  src: loc;
  dst: loc;
  train_idx: Train.Id.t;
  entry: Engine.Text_entry.t;
}
