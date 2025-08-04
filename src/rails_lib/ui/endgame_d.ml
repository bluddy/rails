
type 'state mode =
  | JobOffer of {state:'state Job_offer_d.t; menu: ([ `Quit | `Stay ], 'state) Menu.MsgBox.t option}
  | RetirementBonus of { render_fn: Renderer.window -> 'state -> unit}
  | HallOfFame of Hall_of_fame_d.t

type 'state t = {
  kind: [`RetireEarly | `FinishRun | `Fired];
  mode: 'state mode;
}
