open Sexplib.Std;

[@deriving sexp]
type t =
  | SetFontMetrics(FontMetrics.t);

let apply = (model: Model.t, update: t, _: State.t, ~schedule_action as _) =>
  switch (update) {
  | SetFontMetrics(font_metrics) => {...model, font_metrics}
  };
