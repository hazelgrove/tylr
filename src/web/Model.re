type t = {
  edit_state: EditState.t,
  font_metrics: FontMetrics.t,
};

let cutoff = (===);

let init = () => (
  EditState.Mode.Normal(([], 0)),
  `Exp((HExp.mk_hole(), None)),
);
