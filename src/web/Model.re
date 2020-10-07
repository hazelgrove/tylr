type t = EditState.t;

let cutoff = (===);

let init = () => (
  EditState.Mode.Normal(([], 0)),
  `Exp((HExp.mk_hole(), None)),
);
