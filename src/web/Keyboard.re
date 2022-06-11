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
    let now_save = a => [
      Update.PerformAction(a),
      Update.Save,
      Update.UpdateDoubleTap(None),
    ];
    // let _frame_sort = Ancestors.sort(zipper.relatives.ancestors);
    //let _ = failwith("todo: update on_keydown handler");
    let updates: list(Update.t) =
      if ((!held(Ctrl) || is_digit(key))
          && !held(Alt)
          && !held(Meta)
          || (held(Ctrl) || held(Meta))
          && (key == "x" || key == "v" || key == "q")) {
        switch (key) {
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
        | _ when is_digit(key) && held(Ctrl) =>
          print_endline("switch");
          print_endline(key);
          [SwitchEditor(int_of_string(key))];
        | "F2" =>
          zipper |> Zipper.show |> print_endline;
          [];
        | "F3" =>
          switch (Settings.s.movement) {
          | Char => Settings.s.movement = Mono
          | Mono => Settings.s.movement = Token
          | Token => Settings.s.movement = Char
          };
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
        | "ArrowLeft" when held(Shift) => now(Select(L))
        | "ArrowRight" when held(Shift) => now(Select(R))
        | "ArrowUp" when held(Shift) => now(Select(U))
        | "ArrowDown" when held(Shift) => now(Select(D))
        | "ArrowLeft" => now(Move(L))
        | "ArrowRight" => now(Move(R))
        | "ArrowUp" => now(Move(U))
        | "ArrowDown" => now(Move(D))
        | "q" when held(Ctrl) || held(Meta) => now(RotateBackpack)
        | "x" when held(Ctrl) || held(Meta) => now(Pick_up)
        | "v" when held(Ctrl) || held(Meta) => now(Put_down)
        | "Tab" => now_save(Put_down)
        | "Backspace" =>
          // TODO(d): check whether selection is empty, only select if so
          now_save(Destruct(Left))
        | "Delete" =>
          // TODO(d): fix broken repeated delete
          now_save(Destruct(Right))
        | "Enter" =>
          //NOTE(andrew): using funky char to avoid weird regexp issues with using \n
          now_save(Insert(Whitespace.linebreak))
        | _ when Form.is_valid_char(key) => now_save(Insert(key))
        // | "Tab" =>
        //   let d = held(Shift) ? Direction.Left : Right;
        //   [MoveToNextHole(d)];
        // | "Enter" =>
        //   switch (zipper) {
        //   | (Restructuring(_), _) => [p(Mark)]
        //   | _ => []
        //   }
        // | "Escape" => [Update.escape()]
        | _ when is_printable(key) => [FailedInput(Unrecognized)]
        | _ => []
        };
      } else if (! Os.is_mac^ && held(Ctrl) && !held(Alt) && !held(Meta)) {
        switch (key) {
        | "z"
        | "Z" => held(Shift) ? [Redo] : [Undo]
        | _ => []
        };
      } else if (Os.is_mac^ && held(Meta) && !held(Alt) && !held(Ctrl)) {
        switch (key) {
        | "z"
        | "Z" => held(Shift) ? [Redo] : [Undo]
        // | "x" =>
        //   switch (zipper) {
        //   | (Pointing(_) | Selecting(_, [], _), _) => [
        //       FailedInput(Failure(Cant_move)),
        //     ]
        //   | (Selecting(_, [_, ..._], _), _) => [p(Mark)]
        //   | (Restructuring((_, rframe)), _) =>
        //     switch (rframe) {
        //     | ([Selection(selection), ..._], _)
        //         when Option.is_some(Selection.is_restructurable(selection)) => [
        //         p(Move(Left)),
        //       ]
        //     | (_, [Selection(selection), ..._])
        //         when Option.is_some(Selection.is_restructurable(selection)) => [
        //         p(Move(Right)),
        //       ]
        //     | _ => [FailedInput(Failure(Cant_move))]
        //     }
        //   }
        // | "v" =>
        //   switch (zipper) {
        //   | (Pointing(_) | Selecting(_), _) => [
        //       FailedInput(Failure(Cant_move)),
        //     ]
        //   | (Restructuring(_), _) => [p(Mark)]
        //   }
        | _ => []
        };
      } else if (held(Alt) && !held(Ctrl) && !held(Meta)) {
        switch (key) {
        | "Alt" => [SetShowNeighborTiles(true)]
        // | "ArrowLeft"
        // | "ArrowRight" => arrow_l_r(key, evt, zipper)
        | _ => []
        };
      } else if (held(Ctrl) && !held(Alt) && !held(Meta)) {
        switch (key) {
        | "t" => [ToggleShowNeighborTiles]
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
