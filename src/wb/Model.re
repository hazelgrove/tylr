open Util;
open Cor;

type t = {
  zipper: Zipper.t,
  history_frame: AltList.a_frame(Zipper.t, Action.t),
  font_metrics: FontMetrics.t,
  logo_font_metrics: FontMetrics.t,
};

let cutoff = (===);

let init = () => {
  zipper: (
    Subject.Pointing((
      [],
      Selem.[
        Tile(Exp(Paren([Num(1), Plus, Num(2)]))),
        Tile(Exp(Plus)),
        Tile(Exp(Num(3))),
      ],
    )),
    Frame.Exp(Root),
  ),
  history_frame: ([], []),
  font_metrics: FontMetrics.init,
  logo_font_metrics: FontMetrics.init,
};
