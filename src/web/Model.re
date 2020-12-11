open Core;

type t = {
  edit_state: EditState.t,
  font_metrics: FontMetrics.t,
};

let cutoff = (===);

let init = () => {
  edit_state: (
    EditState.Mode.Normal(([], 1)),
    `Exp((
      Tile.[
        Operand(
          Paren([
            Operand(HExp.T.Num(NotInHole, 1)),
            BinOp(HExp.T.Plus(NotInHole)),
            Operand(HExp.T.Num(NotInHole, 2)),
          ]),
        ),
        BinOp(HExp.T.Plus(NotInHole)),
        Operand(HExp.T.Num(NotInHole, 3)),
      ],
      None,
    )),
  ),
  font_metrics: FontMetrics.init,
};
