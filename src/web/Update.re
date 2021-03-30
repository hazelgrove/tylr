open Sexplib.Std;
open Core;

[@deriving sexp]
type t =
  | SetFontMetrics(FontMetrics.t)
  | SetLogoFontMetrics(FontMetrics.t)
  | SetTypeFontMetrics(FontMetrics.t)
  | PerformAction(Core.Action.t)
  | SetTypeInfoVisibility(bool)
  | Undo
  | Redo
  | Escape;

let perform = (a, model: Model.t) =>
  switch (Core.Action.perform(a, model.edit_state)) {
  | None =>
    print_endline("failed action");
    model;
  | Some(edit_state) =>
    let (before, _) = model.history_frame;
    {
      ...model,
      edit_state,
      history_frame: ([(a, model.edit_state), ...before], []),
    };
  };

let apply = (model: Model.t, update: t, _: State.t, ~schedule_action as _) =>
  switch (update) {
  | SetFontMetrics(font_metrics) => {...model, font_metrics}
  | SetLogoFontMetrics(logo_font_metrics) => {...model, logo_font_metrics}
  | SetTypeFontMetrics(type_font_metrics) => {...model, type_font_metrics}
  | PerformAction(a) => perform(a, model)
  | SetTypeInfoVisibility(b) => {...model, show_type_info: b}
  | Escape =>
    // TODO restore escape functionality on restructuring
    if (EditState.is_selecting(model.edit_state)) {
      // TODO do this directly instead of via double mark
      // in order to support moving caret to right of selection
      perform(
        Mark,
        perform(Mark, model),
      );
    } else {
      model;
    }
  | Undo =>
    switch (model.history_frame) {
    | ([], _) => model
    | ([(a, prev), (a', prev'), ...before], after)
        when EditState.is_selecting(prev) && EditState.has_no_selection(prev) => {
        ...model,
        edit_state: prev',
        history_frame: (
          before,
          [(a', prev), (a, model.edit_state), ...after],
        ),
      }
    | ([(a, prev), ...before], after) => {
        ...model,
        edit_state: prev,
        history_frame: (before, [(a, model.edit_state), ...after]),
      }
    }
  | Redo =>
    switch (model.history_frame) {
    | (_, []) => model
    | (before, [(a, next), (a', next'), ...after])
        when EditState.is_selecting(next) && EditState.has_no_selection(next) => {
        ...model,
        edit_state: next',
        history_frame: (
          [(a', next), (a, model.edit_state), ...before],
          after,
        ),
      }
    | (before, [(a, next), ...after]) => {
        ...model,
        edit_state: next,
        history_frame: ([(a, model.edit_state), ...before], after),
      }
    }
  };
