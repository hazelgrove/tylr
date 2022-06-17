open Virtual_dom.Vdom;
// open Util;
open Core;

let is_printable = s => Re.Str.(string_match(regexp("^[ -~]$"), s, 0));

let is_digit = s => Re.Str.(string_match(regexp("^[0-9]$"), s, 0));

let handlers = (~inject: Update.t => Event.t, ~zipper: Zipper.t, ~double_tap) => [
  Attr.on_keypress(_ => Event.Prevent_default),
  Attr.on_keyup(evt => {
    let key = JsUtil.get_key(evt);
    let updates: list(Update.t) =
      switch (key) {
      | "Shift" => [] // NOTE: don't change doubletap
      | "Alt" => [SetShowBackpackTargets(false)]
      | _ => [UpdateDoubleTap(None)]
      };
    switch (updates) {
    | [] => Event.Many([])
    | [_, ..._] =>
      Event.(
        Many([
          Prevent_default,
          Stop_propagation,
          ...List.map(inject, updates),
        ])
      )
    };
  }),
  Attr.on_keydown(evt => {
    let key = JsUtil.get_key(evt);
    let held = m => JsUtil.held(m, evt);
    let now = a => [Update.PerformAction(a), Update.UpdateDoubleTap(None)];
    //TODO(andrew): think harder about when/where to save
    let now_save_u = u => Update.[u, Save, UpdateDoubleTap(None)];
    let now_save = a => now_save_u(PerformAction(a));
    // let _frame_sort = Ancestors.sort(zipper.relatives.ancestors);
    //let _ = failwith("todo: update on_keydown handler");
    let updates: list(Update.t) =
      if ((!held(Ctrl) || is_digit(key)) && !held(Alt) && !held(Meta)) {
        switch (key) {
        | "Home" => now(Move(Extreme(Left(ByToken))))
        | "End" => now(Move(Extreme(Right(ByToken))))
        | "ArrowLeft" when held(Shift) =>
          now(Select(Local(Left(ByToken))))
        | "ArrowRight" when held(Shift) =>
          now(Select(Local(Right(ByToken))))
        | "ArrowUp" when held(Shift) => now(Select(Local(Up)))
        | "ArrowDown" when held(Shift) => now(Select(Local(Down)))
        | "ArrowLeft" => now(Move(Local(Left(ByChar))))
        | "ArrowRight" => now(Move(Local(Right(ByChar))))
        | "ArrowUp" => now(Move(Local(Up)))
        | "ArrowDown" => now(Move(Local(Down)))
        | "Tab" => now_save(Put_down) //TODO: if empty, move to next hole
        | "Backspace" =>
          // TODO(d): check whether selection is empty, only select if so
          now_save(Destruct(Left))
        | "Delete" =>
          // TODO(d): fix broken repeated delete
          now_save(Destruct(Right))
        | "Enter" =>
          //NOTE(andrew): using funky char to avoid weird regexp issues with using \n
          now_save(Insert(Whitespace.linebreak))
        | "F2" =>
          zipper |> Zipper.show |> print_endline;
          [];
        | "F4" =>
          print_endline("F4 SAVE");
          [Save];
        | "F6" =>
          print_endline("F6 LOAD");
          [Load];
        | "F8" =>
          print_endline("F8 LOAD DEFAULT");
          [LoadDefault];
        | "Shift" =>
          let cur_ts = JsUtil.date_now()##valueOf;
          switch (double_tap) {
          | None => [UpdateDoubleTap(Some(cur_ts))]
          | Some(past_ts) =>
            if (cur_ts -. past_ts < 400.) {
              [UpdateDoubleTap(None), PerformAction(RotateBackpack)];
            } else {
              [UpdateDoubleTap(Some(cur_ts))];
            }
          };

        | _ when Form.is_valid_char(key) => now_save(Insert(key))
        // | "Escape" => [] // TODO: deselect?
        | _ when is_printable(key) => [FailedInput(Unrecognized)]
        | _ => []
        };
      } else if (! Os.is_mac^
                 && held(Ctrl)
                 && !held(Alt)
                 && !held(Meta)
                 || Os.is_mac^
                 && held(Meta)
                 && !held(Alt)
                 && !held(Ctrl)) {
        // OS-agnostic
        switch (key) {
        | "z"
        | "Z" => now_save_u(held(Shift) ? Redo : Undo)
        | "q" => now(RotateBackpack)
        | "x" => now(Pick_up)
        | "v" => now(Put_down)
        | "a" => now(Move(Extreme(Up))) @ now(Select(Extreme(Down)))
        | "ArrowUp" => now(Move(Extreme(Up)))
        | "ArrowDown" => now(Move(Extreme(Down)))
        | _ when is_digit(key) => [SwitchEditor(int_of_string(key))]
        | _ => []
        };
      } else if (Os.is_mac^ && held(Meta)) {
        // mac-specific
        switch (key) {
        | "ArrowLeft" => now(Move(Extreme(Left(ByToken))))
        | "ArrowRight" => now(Move(Extreme(Right(ByToken))))
        | _ => []
        };
      } else if (Os.is_mac^ && held(Ctrl) && !held(Meta) && !held(Alt)) {
        // mac-specific
        switch (key) {
        | "a" => now(Move(Extreme(Left(ByToken))))
        | "e" => now(Move(Extreme(Right(ByToken))))
        | _ => []
        };
      } else if (held(Alt) && !held(Ctrl) && !held(Meta)) {
        let restricted = Backpack.restricted(zipper.backpack);
        switch (key) {
        | "Alt" => [SetShowBackpackTargets(true), UpdateDoubleTap(None)]
        | "ArrowLeft" =>
          restricted
            ? now(MoveToBackpackTarget(Left(ByToken)))
            : now(Move(Local(Left(ByToken))))
        | "ArrowRight" =>
          restricted
            ? now(MoveToBackpackTarget(Right(ByToken)))
            : now(Move(Local(Right(ByToken))))
        | "ArrowUp" => now(MoveToBackpackTarget(Up))
        | "ArrowDown" => now(MoveToBackpackTarget(Down))
        | _ => []
        };
      } else {
        [];
      };
    switch (updates) {
    | [] => Event.Many([])
    | [_, ..._] =>
      Event.(
        Many([
          Prevent_default,
          Stop_propagation,
          ...List.map(inject, updates),
        ])
      )
    };
  }),
];
