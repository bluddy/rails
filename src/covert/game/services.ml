
type t = {
  textures: Textures.t;
  resources: Resources.t;
  fonts: Fonts.t;
  win: R.window;
  random: Utils.Random.State.t; (* separate random state for UI stuff *)
  sound: Sound.t;
}
