open Sexplib.Std;
open Util;
open Core;

[@deriving sexp]
type t =
  | SetFontMetrics(FontMetrics.t)
  | SetLogoFontMetrics(FontMetrics.t)
  | PerformAction(Action.t)
  | Undo
  | Redo
  | Escape(Direction.t)
  | SetShowNeighborTiles(bool);

let escape = (~d=Direction.Left, ()) => Escape(d);

let perform = (a, model: Model.t) =>
  switch (Action.perform(a, model.zipper)) {
  | None => {...model, history: ActionHistory.failed(a, model.history)}
  | Some(zipper) => {
      ...model,
      zipper,
      history: ActionHistory.succeeded(a, model.zipper, model.history),
    }
  };

let apply = (model: Model.t, update: t, _: State.t, ~schedule_action as _) =>
  switch (update) {
  | SetShowNeighborTiles(b) => {
      ...model,
      history: ActionHistory.clear_just_failed(model.history),
      show_neighbor_tiles: b,
    }
  | SetFontMetrics(font_metrics) => {...model, font_metrics}
  | SetLogoFontMetrics(logo_font_metrics) => {...model, logo_font_metrics}
  | PerformAction(a) => perform(a, model)
  | Escape(d) =>
    // TODO restore escape functionality on restructuring
    switch (model.zipper) {
    | (Selecting(selection, (prefix, suffix)), frame) =>
      let sframe =
        switch (d) {
        | Left => (prefix, Parser.parse_selection(Right, selection @ suffix))
        | Right => (
            Parser.parse_selection(Left, List.rev(selection) @ prefix),
            suffix,
          )
        };
      let (sframe, frame) = Parser.parse_zipper(sframe, frame);
      {
        ...model,
        history: ActionHistory.escaped(model.history),
        zipper: (Pointing(sframe), frame),
      };
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
