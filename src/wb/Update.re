open Cor;

[@deriving sexp]
type t =
  | SetFontMetrics(FontMetrics.t)
  | SetLogoFontMetrics(FontMetrics.t)
  | PerformAction(Action.t)
  | Undo
  | Redo
  | Escape(Direction.t);

let escape = (~d=Direction.Left, ()) => Escape(d);

let perform = (a, model: Model.t) =>
  switch (Action.perform(a, model.zipper)) {
  | None =>
    print_endline("failed action");
    model;
  | Some(zipper) =>
    let (before, _) = model.history_frame;
    {
      ...model,
      zipper,
      history_frame: ([(a, model.zipper), ...before], []),
    };
  };

let apply = (model: Model.t, update: t, _: State.t, ~schedule_action as _) =>
  switch (update) {
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
      {...model, zipper: (Pointing(sframe), frame)};
    | _ => model
    }
  | Undo =>
    switch (model.history_frame) {
    | ([], _) => model
    | (
        [
          (a, (Selecting(_), _) as prev),
          (a', (Pointing(_) | Selecting([], _), _) as prev'),
          ...before,
        ],
        after,
      ) => {
        ...model,
        zipper: prev',
        history_frame: (before, [(a', prev), (a, model.zipper), ...after]),
      }
    | ([(a, prev), ...before], after) => {
        ...model,
        zipper: prev,
        history_frame: (before, [(a, model.zipper), ...after]),
      }
    }
  | Redo =>
    switch (model.history_frame) {
    | (_, []) => model
    | (
        before,
        [
          (a, (Selecting(_), _) as next),
          (a', (Pointing(_) | Selecting([], _), _) as next'),
          ...after,
        ],
      ) => {
        ...model,
        zipper: next',
        history_frame: ([(a', next), (a, model.zipper), ...before], after),
      }
    | (before, [(a, next), ...after]) => {
        ...model,
        zipper: next,
        history_frame: ([(a, model.zipper), ...before], after),
      }
    }
  };
