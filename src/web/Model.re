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
        Operand(HExp.Tile.OperandHole),
        BinOp(HExp.Tile.OperatorHole),
        Operand(
          Paren([
            Operand(HExp.Tile.Num(NotInHole, 1)),
            BinOp(HExp.Tile.Plus(NotInHole)),
            Operand(HExp.Tile.Num(NotInHole, 2)),
          ]),
        ),
      ],
      None,
    )),
  ),
  font_metrics: FontMetrics.init,
};
