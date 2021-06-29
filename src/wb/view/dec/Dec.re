open Virtual_dom.Vdom;

module Profile = {
  type t =
    | Caret(CaretDec.Profile.t)
    | CaretPos(CaretPosDec.Profile.t)
    | Selem(SelemDec.Profile.t)
    | Rail(RailDec.Profile.t)
    | UniChild(UniChildDec.Profile.t)
    | RestructuringGenie(RestructuringGenieDec.Profile.t);
};

let view = (~font_metrics: FontMetrics.t, profile: Profile.t): Node.t =>
  switch (profile) {
  | Caret(profile) => CaretDec.view(~font_metrics, profile)
  | CaretPos(profile) => CaretPosDec.view(~font_metrics, profile)
  | Selem(profile) => SelemDec.view(~font_metrics, profile)
  | Rail(profile) => RailDec.view(~font_metrics, profile)
  | UniChild(profile) => UniChildDec.view(~font_metrics, profile)
  | RestructuringGenie(profile) =>
    RestructuringGenieDec.view(~font_metrics, profile)
  };
