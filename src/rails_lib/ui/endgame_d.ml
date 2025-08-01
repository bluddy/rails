
type 'state mode =
  | JobOffer of 'state Job_offer_d.t
  | RetireMenu of ([ `Quit | `Stay ], 'state) Menu.MsgBox.t
  | RetirementBonus of { render_fn: Renderer.window -> 'state -> unit}
  | HallOfFame of Hall_of_fame.t

type 'state t = {
  kind: [`RetireEarly | `FinishRun | `Fired];
  mode: 'state mode;
}
