type t = {
  edit_state: EditState.t,
  font_metrics: FontMetrics.t,
};

let cutoff = (===);

let init = () => {
  edit_state: (
    EditState.Mode.Normal(([], 0)),
    `Exp(([Tile.Operand(HExp.Tile.Num(NotInHole, 1))], None)),
  ),
  font_metrics: FontMetrics.init,
};
