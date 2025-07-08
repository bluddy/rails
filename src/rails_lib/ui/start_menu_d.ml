
type 'state mode =
  | Action of ([`NewGame | `LoadGame], 'state) Menu.MsgBox.t
  | LoadGame
  | Region of (Region.t, 'state) Menu.MsgBox.t
  | Difficulty of (B_options.difficulty, 'state) Menu.MsgBox.t
  | Reality of {
      menu: ([B_options.reality_level | `Continue], 'state) Menu.MsgBox.t;
      reality: B_options.RealityLevels.t
    }

type 'state t = {
  mode: 'state mode;
  region: Region.t option;
  difficulty: B_options.difficulty option;
}

