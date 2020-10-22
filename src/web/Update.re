open Sexplib.Std;

[@deriving sexp]
type t =
  | SetFontMetrics(FontMetrics.t)
  | PerformAction(Core.Action.t);

let apply = (model: Model.t, update: t, _: State.t, ~schedule_action as _) =>
  switch (update) {
  | SetFontMetrics(font_metrics) => {...model, font_metrics}
  | PerformAction(a) =>
    switch (Core.Action.perform(a, model.edit_state)) {
    | None =>
      print_endline("failed action");
      model;
    | Some(edit_state) => {...model, edit_state}
    }
  };
