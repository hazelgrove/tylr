open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;
open Tylr_core;
open Model;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  // | Set(settings_action)
  // | UpdateDoubleTap(option(float))
  // | LoadInit
  // | LoadDefault
  // | Load
  // | Save
  // | SwitchEditor(int)
  | Warmup
  | SetFont(Font.t)
  // | SetLogoFont(Font.t)
  | PerformAction(Edit.t)
  // | FailedInput(FailedInput.reason) //TODO(andrew): refactor as failure?
  | Undo
  | Redo;

let handle_key_event = (k: Util.Key.t, ~model as _: Model.t): list(t) => {
  // let zipper = model.zipper;
  // let restricted = Backpack.restricted(zipper.backpack);
  // let now = a => [PerformAction(a), UpdateDoubleTap(None)];
  // let now_save_u = u => [u, Save, UpdateDoubleTap(None)];
  // let now_save = a => now_save_u(PerformAction(a));
  let now = a => [PerformAction(a)];
  let now_save_u = u => [u];
  let now_save = a => now_save_u(PerformAction(a));
  // let print = str => str |> print_endline |> (_ => []);
  // let toggle = m => (m := ! m^) |> (_ => []);
  switch (k) {
  // | {key: U(key), _} =>
  //   switch (key) {
  //   | "Shift" => [] // NOTE: don't update double_tap here
  //   | "Alt" => [SetShowBackpackTargets(false)]
  //   | _ => [UpdateDoubleTap(None)]
  //   }
  // | {key: D(key), sys: _, shift: Down, meta: Up, ctrl: Up, alt: Up}
  //     when is_f_key(key) =>
  //   switch (key) {
  //   | "F1" => print(Log.get_json_update_log_string())
  //   | "F2" => print(Zipper.show(zipper))
  //   | "F3" => toggle(Log.debug_update)
  //   | "F4" => toggle(Log.debug_keystoke)
  //   | "F5" => toggle(Log.debug_zipper)
  //   | "F6" => [Load]
  //   | "F7" => []
  //   | "F8" => []
  //   | "F10" =>
  //     Log.reset_json_log();
  //     [];
  //   | _ => []
  //   }
  | {key: D(key), sys: _, shift, meta: Up, ctrl: Up, alt: Up} =>
    switch (shift, key) {
    | (Up, "ArrowLeft") => now(Move(Step(H(L))))
    | (Up, "ArrowRight") => now(Move(Step(H(R))))
    | (Up, "ArrowUp") => now(Move(Step(V(L))))
    | (Up, "ArrowDown") => now(Move(Step(V(R))))
    | (Up, "Home") => now(Move(Skip(H(L))))
    | (Up, "End") => now(Move(Skip(H(R))))
    | (Up, "Backspace") => now_save(Delete(L))
    | (Up, "Delete") => now_save(Delete(R))
    | (Up, "Escape") => now(Select(Un(L)))
    | (Up, "Tab") => now(Move(Hole(R)))
    // | (Up, "Tab") => now_save(Put_down) //TODO: if empty, move to next hole
    | (Down, "Tab") => now(Move(Hole(L)))
    | (Down, "ArrowLeft") => now(Select(Move(Step(H(L)))))
    | (Down, "ArrowRight") => now(Select(Move(Step(H(R)))))
    | (Down, "ArrowUp") => now(Select(Move(Step(V(L)))))
    | (Down, "ArrowDown") => now(Select(Move(Step(V(R)))))
    // | (_, "Shift") => update_double_tap(model)
    | (_, "Enter") =>
      //TODO(andrew): using funky char to avoid weird regexp issues with using \n
      now_save(Insert("\n"))
    | _ when String.length(key) == 1 =>
      /* TODO(andrew): length==1 is hack to prevent things
         like F5 which are now valid tokens and also weird
         unicode shit which is multichar i guess */
      now_save(Insert(key))
    | _ => []
    }
  | {key: D(key), sys: Mac, shift: Down, meta: Down, ctrl: Up, alt: Up} =>
    switch (key) {
    | "Z"
    | "z" => now_save_u(Redo)
    | "ArrowLeft" => now(Select(Move(Skip(H(L)))))
    | "ArrowRight" => now(Select(Move(Skip(H(R)))))
    | "ArrowUp" => now(Select(Move(Skip(V(L)))))
    | "ArrowDown" => now(Select(Move(Skip(V(R)))))
    | _ => []
    }
  | {key: D(key), sys: PC, shift: Down, meta: Up, ctrl: Down, alt: Up} =>
    switch (key) {
    | "Z"
    | "z" => now_save_u(Redo)
    | "ArrowLeft" => now(Select(Move(Skip(H(L)))))
    | "ArrowRight" => now(Select(Move(Skip(H(R)))))
    | "ArrowUp"
    | "Home" => now(Select(Move(Skip(V(L)))))
    | "ArrowDown"
    | "End" => now(Select(Move(Skip(V(R)))))
    | _ => []
    }
  | {key: D(key), sys: Mac, shift: Up, meta: Down, ctrl: Up, alt: Up} =>
    switch (key) {
    | "z" => now_save_u(Undo)
    // | "x" => now(Pick_up)
    // | "v" => now(Put_down)
    | "a" => now(Move(Skip(V(L)))) @ now(Select(Move(Skip(V(R)))))
    // | _ when is_digit(key) => [SwitchEditor(int_of_string(key))]
    | "ArrowLeft" => now(Move(Skip(H(L))))
    | "ArrowRight" => now(Move(Skip(H(R))))
    | "ArrowUp" => now(Move(Skip(V(L))))
    | "ArrowDown" => now(Move(Skip(V(R))))
    | _ => []
    }
  | {key: D(key), sys: PC, shift: Up, meta: Up, ctrl: Down, alt: Up} =>
    switch (key) {
    | "z" => now_save_u(Undo)
    // | "x" => now(Pick_up)
    // | "v" => now(Put_down)
    | "a" => now(Move(Skip(V(L)))) @ now(Select(Move(Skip(V(R)))))
    // | _ when is_digit(key) => [SwitchEditor(int_of_string(key))]
    | "ArrowLeft" => now(Move(Skip(H(L))))
    | "ArrowRight" => now(Move(Skip(H(R))))
    | "ArrowUp" => now(Move(Skip(V(L))))
    | "ArrowDown" => now(Move(Skip(V(R))))
    | _ => []
    }
  | {key: D(key), sys: Mac, shift: Up, meta: Up, ctrl: Down, alt: Up} =>
    switch (key) {
    | "a" => now(Move(Skip(H(L))))
    | "e" => now(Move(Skip(H(R))))
    | _ => []
    }
  // | {key: D(key), sys, shift: Up, meta: Up, ctrl: Up, alt: Down} =>
  //   switch (sys, key) {
  //   | (_, "ArrowLeft") when restricted =>
  //     now(MoveToBackpackTarget(Left(ByToken)))
  //   | (_, "ArrowRight") when restricted =>
  //     now(MoveToBackpackTarget(Right(ByToken)))
  //   | (Mac, "ArrowLeft") => now(Move(Local(Left(ByToken))))
  //   | (Mac, "ArrowRight") => now(Move(Local(Right(ByToken))))
  //   | (_, "Alt") => [SetShowBackpackTargets(true), UpdateDoubleTap(None)]
  //   | (_, "ArrowUp") => now(MoveToBackpackTarget(Up))
  //   | (_, "ArrowDown") => now(MoveToBackpackTarget(Down))
  //   | _ => []
  //   }
  | _ => []
  };
};

// [@deriving (show, sexp, yojson)]
// type settings_action =
//   | Captions
//   | WhitespaceIcons;

module Failure = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | CantUndo
    | CantRedo
    // | FailedToLoad
    // | FailedToSwitch
    // | UnrecognizedInput(FailedInput.reason)
    | FailedToPerform
    | Exception(string);
};

module Result = {
  include Result;
  type t('success) = Result.t('success, Failure.t);
};

// let save = (model: Model.t): unit =>
//   switch (model.editor_model) {
//   | Simple(z) => LocalStorage.save_syntax(0, z)
//   | Study(n, zs) =>
//     assert(n < List.length(zs));
//     LocalStorage.save_syntax(n, List.nth(zs, n));
//   };

// let update_settings =
//     (a: settings_action, settings: Model.settings): Model.settings => {
//   let settings =
//     switch (a) {
//     | Captions => {...settings, captions: !settings.captions}
//     | WhitespaceIcons => {
//         ...settings,
//         whitespace_icons: !settings.whitespace_icons,
//       }
//     };
//   LocalStorage.save_settings(settings);
//   settings;
// };

// let move_to_start = z =>
//   switch (
//     Zipper.do_extreme(Zipper.move(ByToken, Zipper.from_plane(Up)), Up, z)
//   ) {
//   | Some(z) => Zipper.update_target(z)
//   | None => z
//   };

let apply =
    (model: Model.t, update: t, _: State.t, ~schedule_action as _)
    : Result.t(Model.t) => {
  //print_endline("apply");
  switch (update) {
  | Warmup =>
    Tylr_core.Walker.warmup();
    Ok(model);
  | SetFont(font) => Ok({...model, font})
  // | SetLogoFont(logo_font_metrics) =>
  //   Ok({...model, logo_font_metrics})
  | PerformAction(a) =>
    switch (Edit.perform(a, model.zipper)) {
    | None => Error(FailedToPerform)
    | Some(z) =>
      Ok({...model, zipper: z, history: History.do_(a, z, model.history)})
    }
  // | FailedInput(reason) => Error(UnrecognizedInput(reason))
  | Undo =>
    switch (History.undo(model.zipper, model.history)) {
    | None => Error(CantUndo)
    | Some((zipper, history)) => Ok({...model, zipper, history})
    }
  | Redo =>
    switch (History.redo(model.zipper, model.history)) {
    | None => Error(CantRedo)
    | Some((zipper, history)) => Ok({...model, zipper, history})
    }
  // | Set(s_action) =>
  //   Ok({...model, settings: update_settings(s_action, model.settings)})
  // | LoadInit =>
  //   let (zs, id_gen) =
  //     List.fold_left(
  //       ((z_acc, id_gen: IdGen.state), n) =>
  //         switch (LocalStorage.load_syntax(n, id_gen)) {
  //         | Some((z, id_gen)) => (z_acc @ [z], id_gen)
  //         | None => (z_acc @ [Model.empty_zipper], id_gen)
  //         },
  //       ([], model.id_gen),
  //       List.init(LocalStorage.num_editors, n => n),
  //     );
  //   let zs = List.map(move_to_start, zs);
  //   Ok({
  //     ...model,
  //     history: History.empty,
  //     id_gen,
  //     settings: LocalStorage.load_settings(),
  //     editor_model: Study(LocalStorage.load_editor_idx(), zs),
  //   });
  // | LoadDefault =>
  //   let n = Model.current_editor(model);
  //   switch (LocalStorage.load_default_syntax(n, model.id_gen)) {
  //   | Some((z, id_gen)) =>
  //     Ok({
  //       ...model,
  //       history: History.empty,
  //       editor_model: Model.put_zipper(model, move_to_start(z)),
  //       id_gen,
  //     })
  //   | None => Error(FailedToLoad)
  //   };
  // | Load =>
  //   let n = Model.current_editor(model);
  //   switch (LocalStorage.load_syntax(n, model.id_gen)) {
  //   | Some((z, id_gen)) =>
  //     Ok({
  //       ...model,
  //       history: History.empty,
  //       editor_model: Model.put_zipper(model, move_to_start(z)),
  //       id_gen,
  //     })
  //   | None => Error(FailedToLoad)
  //   };
  // | Save =>
  //   save(model);
  //   Ok(model);
  // | SwitchEditor(n) =>
  //   switch (model.editor_model) {
  //   | Simple(_) => Error(FailedToSwitch)
  //   | Study(m, _) when m == n => Error(FailedToSwitch)
  //   | Study(_, zs) =>
  //     switch (n < List.length(zs)) {
  //     | false => Error(FailedToSwitch)
  //     | true =>
  //       assert(n < List.length(zs));
  //       LocalStorage.save_editor_idx(n);
  //       Ok({
  //         ...model,
  //         history: History.empty,
  //         editor_model: Study(n, zs),
  //       });
  //     }
  //   }
  };
};
