let is_printable = s => Re.Str.(string_match(regexp("^[ -~]$"), s, 0));
let is_digit = s => Re.Str.(string_match(regexp("^[0-9]$"), s, 0));
let is_f_key = s => Re.Str.(string_match(regexp("^F[0-9][0-9]*$"), s, 0));

// let update_double_tap = (model: Model.t): list(Update.t) => {
//   let cur_time = JsUtil.timestamp();
//   switch (model.double_tap) {
//   | None => [UpdateDoubleTap(Some(cur_time))]
//   | Some(prev_time) =>
//     if (cur_time -. prev_time < 400.) {
//       [UpdateDoubleTap(None), PerformAction(RotateBackpack)];
//     } else {
//       [UpdateDoubleTap(Some(cur_time))];
//     }
//   };
// };

let handle_key_event = (k: Key.t, ~model as _: Model.t): list(Update.t) => {
  // let zipper = model.zipper;
  // let restricted = Backpack.restricted(zipper.backpack);
  // let now = a => [Update.PerformAction(a), Update.UpdateDoubleTap(None)];
  // let now_save_u = u => Update.[u, Save, UpdateDoubleTap(None)];
  // let now_save = a => now_save_u(PerformAction(a));
  let now = a => [Update.PerformAction(a)];
  let now_save_u = u => [u];
  let now_save = a => now_save_u(Update.PerformAction(a));
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
    // | (Up, "Tab") => now_save(Put_down) //TODO: if empty, move to next hole
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
    | "z" => now_save_u(Update.Redo)
    | "ArrowLeft" => now(Select(Move(Skip(H(L)))))
    | "ArrowRight" => now(Select(Move(Skip(H(R)))))
    | "ArrowUp" => now(Select(Move(Skip(V(L)))))
    | "ArrowDown" => now(Select(Move(Skip(V(R)))))
    | _ => []
    }
  | {key: D(key), sys: PC, shift: Down, meta: Up, ctrl: Down, alt: Up} =>
    switch (key) {
    | "Z"
    | "z" => now_save_u(Update.Redo)
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
    | "z" => now_save_u(Update.Undo)
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
    | "z" => now_save_u(Update.Undo)
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
