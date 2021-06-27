open Virtual_dom.Vdom;

type profile =
  | Caret(CaretDec.Profile.t);

let view = (~font_metrics: FontMetrics.t, profile: profile): Node.t =>
  switch (profile) {
  | Caret(profile) => CaretDec.view(~font_metrics, profile)
  };
