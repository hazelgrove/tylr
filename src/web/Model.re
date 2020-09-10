type t = Action.Exp.edit_state;

let cutoff = (===);

let init = () => Action.Exp.Normal({focus: ZExp.mk_hole()});
