open Core;

type t = {
  zipper: Zipper.t,
  history: ActionHistory.t,
  font_metrics: FontMetrics.t,
  logo_font_metrics: FontMetrics.t,
};

let cutoff = (===);

let init = () => {
  zipper: (
    Subject.Pointing((
      [],
      Selem.[
        Tile(Exp(Let([Var("p")], [Num(1), Prod, Num(2)]))),
        Tile(
          Exp(
            Let(
              [Var("f")],
              [
                Lam([Paren([Var("x"), Prod, Var("y")]), Prod, Var("z")]),
                Var("x"),
                Plus,
                Var("y"),
                Times,
                Var("z"),
              ],
            ),
          ),
        ),
        Tile(Exp(Var("f"))),
        Tile(Exp(Ap)),
        Tile(Exp(Paren([Var("p"), Prod, Num(3)]))),
      ],
    )),
    Frame.Exp(Root),
  ),
  history: ActionHistory.empty,
  font_metrics: FontMetrics.init,
  logo_font_metrics: FontMetrics.init,
};

let filler = (model: t) => {
  switch (model.zipper) {
  | (Pointing(_) | Selecting(_), _) => 0
  | (Restructuring(_), _) =>
    switch (ActionHistory.zipper_before_restructuring(model.history)) {
    | None => 0
    | Some(zipper) =>
      let len_before = Layout.length(Layout.mk_zipper(zipper));
      let len_now = Layout.length(Layout.mk_zipper(model.zipper));
      len_now < len_before ? len_before - len_now : 0;
    }
  };
};
