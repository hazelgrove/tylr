open Virtual_dom.Vdom;

module Profile = {
  type t =
    | Caret(CaretDec.Profile.t)
    | CaretPos(CaretPosDec.Profile.t)
    | RestructuringGenie(RestructuringGenieDec.Profile.t);
};

let view = (~font_metrics: FontMetrics.t, profile: Profile.t): Node.t =>
  switch (profile) {
  | Caret(profile) => CaretDec.view(~font_metrics, profile)
  | CaretPos(profile) => CaretPosDec.view(~font_metrics, profile)
  | RestructuringGenie(profile) =>
    RestructuringGenieDec.view(~font_metrics, profile)
  };
