open Sexplib.Std;
open New;

[@deriving sexp]
type t =
  | SetFontMetrics(FontMetrics.t)
  | SetLogoFontMetrics(FontMetrics.t)
  | PerformAction(New.Action.t)
  | Escape;

let perform = (a, model: Model.t) =>
  switch (New.Action.perform(a, model.edit_state)) {
  | None =>
    print_endline("failed action");
    model;
  | Some(edit_state) => {...model, edit_state}
  };

let apply = (model: Model.t, update: t, _: State.t, ~schedule_action as _) =>
  switch (update) {
  | SetFontMetrics(font_metrics) => {...model, font_metrics}
  | SetLogoFontMetrics(logo_font_metrics) => {...model, logo_font_metrics}
  | PerformAction(a) => perform(a, model)
  | Escape =>
    // TODO restore escape functionality on restructuring
    if (EditState.is_selecting(model.edit_state)) {
      perform(Mark, perform(Mark, model));
    } else {
      model;
    }
  };
