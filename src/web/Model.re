open Util;
open Core;

type t = {
  edit_state: EditState.t,
  history_frame: AltList.a_frame(EditState.t, Action.t),
  show_type_info: bool,
  font_metrics: FontMetrics.t,
  logo_font_metrics: FontMetrics.t,
};

let cutoff = (===);

let init = () => {
  edit_state:
    Term_exp.(
      Exp((
        Pointing((
          [],
          Tile.[
            Op(Paren(Bin(Op(Num(1)), Plus, Op(Num(2))))),
            Bin(Plus),
            Op(Num(3)),
          ],
        )),
        Frame_exp.Root,
      ))
    ),
  history_frame: ([], []),
  show_type_info: false,
  font_metrics: FontMetrics.init,
  logo_font_metrics: FontMetrics.init,
};
