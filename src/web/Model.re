type t = Action.EditState.t;

let cutoff = (===);

let init = () => (Action.EditMode.Normal(([], 0)), (HExp.mk_hole(), None));
