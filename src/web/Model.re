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
        Op(
          Paren([
            Op(HExp.Num(NotInHole, 1)),
            Bin(HExp.Plus(NotInHole)),
            Op(HExp.Num(NotInHole, 2)),
          ]),
        ),
        Bin(HExp.Plus(NotInHole)),
        Op(HExp.Num(NotInHole, 3)),
      ],
      None,
    )),
  ),
  font_metrics: FontMetrics.init,
};
