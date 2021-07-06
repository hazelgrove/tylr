open Util;
open Core;

[@deriving sexp]
type t =
  | SetFontMetrics(FontMetrics.t)
  | SetLogoFontMetrics(FontMetrics.t)
  | PerformAction(Action.t)
  | UnrecognizedInput
  | Undo
  | Redo
  | Escape(Direction.t);

let escape = (~d=Direction.Left, ()) => Escape(d);

let perform = (a, model: Model.t) =>
  switch (Action.perform(a, model.zipper)) {
  | Error(failure) =>
    print_endline("failed action");
    {...model, history: ActionHistory.failure(failure, model.history)};
  | Ok(zipper) => {
      ...model,
      zipper,
      history: ActionHistory.succeeded(a, model.zipper, model.history),
    }
  };

let apply = (model: Model.t, update: t, _: State.t, ~schedule_action as _) =>
  switch (update) {
  | SetFontMetrics(font_metrics) => {...model, font_metrics}
  | SetLogoFontMetrics(logo_font_metrics) => {...model, logo_font_metrics}
  | PerformAction(a) => perform(a, model)
  | UnrecognizedInput => {
      ...model,
      history: ActionHistory.unrecognized_input(model.history),
    }
  | Escape(d) =>
    // TODO restore escape functionality on restructuring
    switch (model.zipper) {
    | (Selecting(_, selection, (prefix, suffix)), frame) =>
      let sframe =
        switch (d) {
        | Left => (prefix, Parser.parse_selection(Right, selection @ suffix))
        | Right => (
            Parser.parse_selection(Left, List.rev(selection) @ prefix),
            suffix,
          )
        };
      let (sframe, frame) = Parser.parse_zipper(sframe, frame);
      {...model, zipper: (Pointing(sframe), frame)};
    | _ => model
    }
  | Undo =>
    switch (ActionHistory.undo(model.zipper, model.history)) {
    | None => model
    | Some((zipper, history)) => {...model, zipper, history}
    }
  | Redo =>
    switch (ActionHistory.redo(model.zipper, model.history)) {
    | None => model
    | Some((zipper, history)) => {...model, zipper, history}
    }
  };
