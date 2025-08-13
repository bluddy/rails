
type 'state t = {
  mutable last_time: int;
  mutable ctr: int;
  station_loc: Utils.loc option;
  newspaper: 'state Newspaper_d.t;
}
