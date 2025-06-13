
type loc = Utils.loc

type t = {
  speed: int;
  src: loc;
  dst: loc;
  train_idx: Train.Id.t;
  entry: Text_entry.t;
}
