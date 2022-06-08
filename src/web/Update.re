open Sexplib.Std;
open Util;
open Core;

[@deriving sexp]
type t =
  | SwitchEditor(int)
  | SetFontMetrics(FontMetrics.t)
  | SetLogoFontMetrics(FontMetrics.t)
  | PerformAction(Zipper.Action.t)
  | FailedInput(FailedInput.reason)
  | Undo
  | Redo
  | Escape(Direction.t)
  | SetShowNeighborTiles(bool)
  | ToggleShowNeighborTiles
  | MoveToNextHole(Direction.t);

let escape = (~d=Direction.Left, ()) => Escape(d);

// let update_result =
//     (a, result: Result.t(Zipper.t, Failure.t), model: Model.t) =>
//   switch (result) {
//   | Error(failure) => {
//       ...model,
//       history: ActionHistory.failure(failure, model.history),
//     }
//   | Ok(zipper) => {
//       ...model,
//       zipper,
//       history: ActionHistory.succeeded(a, model.zipper, model.history),
//     }
//   };

let apply = (model: Model.t, update: t, _: State.t, ~schedule_action as _) => {
  //print_endline("Update.apply");
  switch (update) {
  | SwitchEditor(n) =>
    switch (model.editor_model) {
    | Simple(_) =>
      print_endline("Can't switch");
      model;
    | Study(_, zs) =>
      assert(n < List.length(zs));
      {...model, editor_model: Study(n, zs)};
    }
  | SetShowNeighborTiles(b) => {
      ...model,
      history: ActionHistory.clear_just_failed(model.history),
      show_neighbor_tiles: b,
    }
  | ToggleShowNeighborTiles => {
      ...model,
      history: ActionHistory.clear_just_failed(model.history),
      show_neighbor_tiles: !model.show_neighbor_tiles,
    }
  | SetFontMetrics(font_metrics) => {...model, font_metrics}
  | SetLogoFontMetrics(logo_font_metrics) => {...model, logo_font_metrics}
  | PerformAction(action) =>
    model
    |> Model.update_zipper(z_id =>
         switch (Zipper.perform(action, z_id)) {
         | Error(err) =>
           print_endline(Zipper.Action.Failure.show(err));
           z_id;
         | Ok(r) => r
         }
       )
  | FailedInput(reason) => {
      ...model,
      history: ActionHistory.just_failed(reason, model.history),
    }
  | Escape(_d) =>
    // TODO restore escape functionality on restructuring
    // switch (model.zipper) {
    // | (Selecting(_, selection, (prefix, suffix)), frame) =>
    //   let sframe =
    //     switch (d) {
    //     | Left => (prefix, Parser.parse_selection(Right, selection @ suffix))
    //     | Right => (
    //         Parser.parse_selection(Left, List.rev(selection) @ prefix),
    //         suffix,
    //       )
    //     };
    //   let (sframe, frame) = Parser.parse_zipper(sframe, frame);
    //   {
    //     ...model,
    //     history: ActionHistory.escaped(model.history),
    //     zipper: (Pointing(sframe), frame),
    //   };
    // | _ => model
    // }
    model
  | Undo =>
    // switch (ActionHistory.undo(model.zipper, model.history)) {
    // | None => model
    // | Some((zipper, history)) => {...model, zipper, history}
    // }
    model
  | Redo =>
    // switch (ActionHistory.redo(model.zipper, model.history)) {
    // | None => model
    // | Some((zipper, history)) => {...model, zipper, history}
    // }
    model
  | MoveToNextHole(_d) =>
    // let moved = Action.move_to_next_hole(d, model.zipper);
    // // Move(d) is hack arg, doesn't affect undo behavior
    // update_result(Move(d), moved, model);
    model
  };
};
