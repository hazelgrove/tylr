open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;
open Tylr_core;
open Model;

let catch_exns = ref(true);

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Warmup
  | SetFont(Font.t)
  | PerformAction(Edit.t)
  | Undo
  | Redo
  | Load(int);

let is_f_key = s => Re.Str.(string_match(regexp("^F[0-9][0-9]*$"), s, 0));

let handle_key_event = (k: Util.Key.t, ~model: Model.t): list(t) => {
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
  //   if (key == "F12") {
  //     print_endline("Catch exceptions: " ++ string_of_bool(! catch_exns^));
  //     catch_exns := ! catch_exns^;
  //     [];
  //   } else {
  //     let index =
  //       int_of_string(Base.String.chop_prefix_exn(~prefix="F", key)) - 1;
  //     print_endline("F key pressed: index = " ++ string_of_int(index));
  //     now_save_u(Load(index));
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
    | (Up, "Tab") => now(Tab(R))
    // | (Up, "Tab") => now_save(Put_down) //TODO: if empty, move to next hole
    | (Down, "Tab") => now(Tab(L))
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
    | "s" =>
      print_endline(Store.serialize(model.zipper));
      [];
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

module Failure = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | CantUndo
    | CantRedo
    | FailedToPerform
    | Exception(string);
};

module Result = {
  include Result;
  type t('success) = Result.t('success, Failure.t);
};

let apply =
    (model: Model.t, update: t, _: State.t, ~schedule_action as _)
    : Result.t(Model.t) => {
  // print_endline("apply");
  switch (update) {
  | SetFont(font) => Ok({...model, font})
  | PerformAction(a) =>
    switch (Edit.perform(a, model.zipper)) {
    | None => Error(FailedToPerform)
    | Some(z) =>
      Ok({
        ...model,
        zipper: z,
        history: History.do_(a, model.zipper, model.history),
      })
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
  | Load(n) =>
    Ok({
      ...model,
      editor: n,
      zipper: Store.load_default_syntax(n),
      history: History.empty,
      hist: [],
    })
  //TODO: remove when warmup is finalized
  // | Warmup =>
  //   Tylr_core.Warmup.warmup();
  //   Ok(model);
  | Warmup => Error(FailedToPerform)
  };
};
