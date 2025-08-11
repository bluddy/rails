
type 'state mode =
  | JobOffer of {state:'state Job_offer_d.t; menu: ([ `Quit | `DontQuit ], 'state) Menu.MsgBox.t option}
  | RetirementBonus of { render_fn: Renderer.window -> 'state -> unit}
  | HallOfFame of Hall_of_fame_d.t
  | Advert of {render_fn: Renderer.window -> 'state -> unit}

type 'state t = {
  kind: [`RetireEarly | `FinishRun | `Fired];
  mode: 'state mode;
}
