open Sexplib.Std;
open Util;
open Core;

[@deriving sexp]
type t =
  | UpdateDoubleTap(option(float))
  | LoadInit
  | Load
  | LoadDefault
  | Save
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

let save = (model: Model.t): unit =>
  switch (model.editor_model) {
  | Simple(z) => LocalStorage.save_syntax(0, z)
  | Study(n, zs) =>
    assert(n < List.length(zs));
    LocalStorage.save_syntax(n, List.nth(zs, n));
  };

let current_editor = (model: Model.t): int =>
  switch (model.editor_model) {
  | Simple(_) => 0
  | Study(n, zs) =>
    assert(n < List.length(zs));
    n;
  };

let apply = (model: Model.t, update: t, _: State.t, ~schedule_action as _) => {
  //print_endline("Update.apply");
  switch (update) {
  | UpdateDoubleTap(double_tap) => {...model, double_tap}
  | LoadInit =>
    let num_editors = LocalStorage.num_editors;
    let init_editor = LocalStorage.load_editor_idx();
    let (zs, id_gen) =
      List.fold_left(
        ((z_acc, id_gen: IdGen.state), n) =>
          switch (LocalStorage.load_syntax(n, id_gen)) {
          | Some((z, id_gen)) => (z_acc @ [z], id_gen)
          | None => (z_acc @ [Model.empty_zipper], id_gen)
          },
        ([], model.id_gen),
        List.init(num_editors, n => n),
      );
    {
      ...model,
      history: ActionHistory.empty,
      id_gen,
      editor_model: Study(init_editor, zs),
    };
  | Load =>
    let n = current_editor(model);
    switch (LocalStorage.load_syntax(n, model.id_gen)) {
    | Some((z, id_gen)) => {
        ...model,
        history: ActionHistory.empty,
        editor_model: Model.put_zipper(model, z),
        id_gen,
      }
    | None => model
    };
  | LoadDefault =>
    let n = current_editor(model);
    switch (LocalStorage.load_default_syntax(n, model.id_gen)) {
    | Some((z, id_gen)) => {
        ...model,
        history: ActionHistory.empty,
        editor_model: Model.put_zipper(model, z),
        id_gen,
      }
    | None => model
    };
  | Save =>
    save(model);
    model;
  | SwitchEditor(n) =>
    switch (model.editor_model) {
    | Simple(_) =>
      print_endline("Can't switch");
      model;
    | Study(m, _) when m == n => model
    | Study(_, zs) =>
      assert(n < List.length(zs));
      LocalStorage.save_editor_idx(n);
      {...model, history: ActionHistory.empty, editor_model: Study(n, zs)};
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
  | PerformAction(a) =>
    let z_id = (Model.get_zipper(model), model.id_gen);
    switch (Zipper.perform(a, z_id)) {
    | Error(err) =>
      print_endline(Zipper.Action.Failure.show(err));
      {...model, history: ActionHistory.failure(err, model.history)};
    | Ok((z, id_gen)) => {
        ...model,
        editor_model: Model.put_zipper(model, z),
        id_gen,
        history: ActionHistory.succeeded(a, z_id, model.history),
      }
    };
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
    let z_id = (Model.get_zipper(model), model.id_gen);
    switch (ActionHistory.undo(z_id, model.history)) {
    | None => model
    | Some(((z, id_gen), history)) => {
        ...model,
        editor_model: Model.put_zipper(model, z),
        id_gen,
        history,
      }
    };
  | Redo =>
    let z_id = (Model.get_zipper(model), model.id_gen);
    switch (ActionHistory.redo(z_id, model.history)) {
    | None => model
    | Some(((z, id_gen), history)) => {
        ...model,
        editor_model: Model.put_zipper(model, z),
        id_gen,
        history,
      }
    };
  | MoveToNextHole(_d) =>
    // let moved = Action.move_to_next_hole(d, model.zipper);
    // // Move(d) is hack arg, doesn't affect undo behavior
    // update_result(Move(d), moved, model);
    model
  };
};
