open Sexplib.Std;
open Util;
open Core;

[@deriving sexp]
type settings_action =
  | Captions
  | WhitespaceIcons;

[@deriving sexp]
type t =
  | Set(settings_action)
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
  | SetShowBackpackTargets(bool)
  | MoveToNextHole(Direction.t);

let escape = (~d=Direction.Left, ()) => Escape(d);

let save = (model: Model.t): unit =>
  switch (model.editor_model) {
  | Simple(z) => LocalStorage.save_syntax(0, z)
  | Study(n, zs) =>
    assert(n < List.length(zs));
    LocalStorage.save_syntax(n, List.nth(zs, n));
  };

let update_settings =
    (a: settings_action, settings: Model.settings): Model.settings => {
  let settings =
    switch (a) {
    | Captions => {...settings, captions: !settings.captions}
    | WhitespaceIcons => {
        ...settings,
        whitespace_icons: !settings.whitespace_icons,
      }
    };
  LocalStorage.save_settings(settings);
  settings;
};

let move_to_start = z =>
  switch (
    Zipper.do_extreme(Zipper.move(ByToken, Zipper.from_plane(Up)), Up, z)
  ) {
  | Some(z) => Zipper.update_target(z)
  | None => z
  };

let apply = (model: Model.t, update: t, _: State.t, ~schedule_action as _) => {
  //print_endline("Update.apply");
  switch (update) {
  | Set(s_action) => {
      ...model,
      settings: update_settings(s_action, model.settings),
    }
  | UpdateDoubleTap(double_tap) => {...model, double_tap}
  | LoadInit =>
    let (zs, id_gen) =
      List.fold_left(
        ((z_acc, id_gen: IdGen.state), n) =>
          switch (LocalStorage.load_syntax(n, id_gen)) {
          | Some((z, id_gen)) => (z_acc @ [z], id_gen)
          | None => (z_acc @ [Model.empty_zipper], id_gen)
          },
        ([], model.id_gen),
        List.init(LocalStorage.num_editors, n => n),
      );
    let zs = List.map(move_to_start, zs);
    {
      ...model,
      history: ActionHistory.empty,
      id_gen,
      settings: LocalStorage.load_settings(),
      editor_model: Study(LocalStorage.load_editor_idx(), zs),
    };
  | Load =>
    let n = Model.current_editor(model);
    switch (LocalStorage.load_syntax(n, model.id_gen)) {
    | Some((z, id_gen)) => {
        ...model,
        history: ActionHistory.empty,
        editor_model: Model.put_zipper(model, move_to_start(z)),
        id_gen,
      }
    | None => model
    };
  | LoadDefault =>
    let n = Model.current_editor(model);
    switch (LocalStorage.load_default_syntax(n, model.id_gen)) {
    | Some((z, id_gen)) => {
        ...model,
        history: ActionHistory.empty,
        editor_model: Model.put_zipper(model, move_to_start(z)),
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
  | SetShowBackpackTargets(b) => {
      ...model,
      history: ActionHistory.clear_just_failed(model.history),
      show_backpack_targets: b,
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
  | Escape(_d) => model
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
    // TODO restore
    model
  };
};
