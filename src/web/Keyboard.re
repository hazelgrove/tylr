open Core;

let is_printable = s => Re.Str.(string_match(regexp("^[ -~]$"), s, 0));

let is_digit = s => Re.Str.(string_match(regexp("^[0-9]$"), s, 0));

let handle_key_event = (k: Key.t, ~model): list(Update.t) => {
  let zipper = Model.get_zipper(model);
  let restricted = Backpack.restricted(zipper.backpack);
  let double_tap = model.double_tap;
  let now = a => [Update.PerformAction(a), Update.UpdateDoubleTap(None)];
  let now_save_u = u => Update.[u, Save, UpdateDoubleTap(None)];
  let now_save = a => now_save_u(PerformAction(a));
  switch (k) {
  | (U(key), _, _, _, _, _) =>
    switch (key) {
    | "Shift" => [] // NOTE: don't change doubletap
    | "Alt" => [Update.SetShowBackpackTargets(false)]
    | _ => [UpdateDoubleTap(None)]
    }
  | (D(key), _, Shift(shift), Meta(Up), Ctrl(Up), Alt(Up)) =>
    switch (shift, key) {
    | (Up, "ArrowLeft") => now(Move(Local(Left(ByChar))))
    | (Up, "ArrowRight") => now(Move(Local(Right(ByChar))))
    | (Up, "ArrowUp") => now(Move(Local(Up)))
    | (Up, "ArrowDown") => now(Move(Local(Down)))
    | (Up, "Home") => now(Move(Extreme(Left(ByToken))))
    | (Up, "End") => now(Move(Extreme(Right(ByToken))))
    | (Up, "Backspace") => now_save(Destruct(Left))
    | (Up, "Delete") => now_save(Destruct(Right))
    | (Up, "Escape") => now(Unselect)
    | (Up, "Tab") => now_save(Put_down) //TODO: if empty, move to next hole
    | (Up, "F2") => zipper |> Zipper.show |> print_endline |> (_ => [])
    | (Up, "F4") => [Save]
    | (Up, "F6") => [Load]
    | (Up, "F8") => [LoadDefault, Save]
    | (Down, "ArrowLeft") => now(Select(Local(Left(ByToken))))
    | (Down, "ArrowRight") => now(Select(Local(Right(ByToken))))
    | (Down, "ArrowUp") => now(Select(Local(Up)))
    | (Down, "ArrowDown") => now(Select(Local(Down)))
    | (_, "Shift") =>
      let cur_ts = JsUtil.timestamp();
      switch (double_tap) {
      | None => [UpdateDoubleTap(Some(cur_ts))]
      | Some(past_ts) =>
        if (cur_ts -. past_ts < 400.) {
          [UpdateDoubleTap(None), PerformAction(RotateBackpack)];
        } else {
          [UpdateDoubleTap(Some(cur_ts))];
        }
      };
    | (_, "Enter") =>
      //TODO(andrew): using funky char to avoid weird regexp issues with using \n
      now_save(Insert(Whitespace.linebreak))
    | _ when Form.is_valid_char(key) && String.length(key) == 1 =>
      /* TODO(andrew): length==1 is hack to prevent things
         like F5 which are now valid tokens and also weird
         unicode shit which is multichar i guess */
      now_save(Insert(key))
    | _ => []
    }
  | (D(key), IsMac, Shift(Down), Meta(Down), Ctrl(Up), Alt(Up)) =>
    switch (key) {
    | "Z" => now_save_u(Redo)
    | "ArrowLeft" => now(Select(Extreme(Left(ByToken))))
    | "ArrowRight" => now(Select(Extreme(Right(ByToken))))
    | "ArrowUp" => now(Select(Extreme(Up)))
    | "ArrowDown" => now(Select(Extreme(Down)))
    | _ => []
    }
  | (D(key), IsPC, Shift(Down), Meta(Up), Ctrl(Down), Alt(Up)) =>
    switch (key) {
    | "Z" => now_save_u(Redo)
    | "ArrowLeft" => now(Select(Local(Left(ByToken))))
    | "ArrowRight" => now(Select(Local(Right(ByToken))))
    | "ArrowUp" => now(Select(Local(Up)))
    | "ArrowDown" => now(Select(Local(Down)))
    | "Home" => now(Select(Extreme(Up)))
    | "End" => now(Select(Extreme(Down)))
    | _ => []
    }
  | (D(key), IsMac, Shift(Up), Meta(Down), Ctrl(Up), Alt(Up)) =>
    switch (key) {
    | "z" => now_save_u(Undo)
    | "x" => now(Pick_up)
    | "v" => now(Put_down)
    | "a" => now(Move(Extreme(Up))) @ now(Select(Extreme(Down)))
    | _ when is_digit(key) => [SwitchEditor(int_of_string(key))]
    | "ArrowLeft" => now(Move(Extreme(Left(ByToken))))
    | "ArrowRight" => now(Move(Extreme(Right(ByToken))))
    | "ArrowUp" => now(Move(Extreme(Up)))
    | "ArrowDown" => now(Move(Extreme(Down)))
    | _ => []
    }
  | (D(key), IsPC, Shift(Up), Meta(Up), Ctrl(Down), Alt(Up)) =>
    switch (key) {
    | "z" => now_save_u(Undo)
    | "x" => now(Pick_up)
    | "v" => now(Put_down)
    | "a" => now(Move(Extreme(Up))) @ now(Select(Extreme(Down)))
    | _ when is_digit(key) => [SwitchEditor(int_of_string(key))]
    | "ArrowLeft" => now(Move(Local(Left(ByToken))))
    | "ArrowRight" => now(Move(Local(Right(ByToken))))
    | "Home" => now(Move(Extreme(Up)))
    | "End" => now(Move(Extreme(Down)))
    | _ => []
    }
  | (D(key), IsMac, Shift(Up), Meta(Up), Ctrl(Down), Alt(Up)) =>
    switch (key) {
    | "a" => now(Move(Extreme(Left(ByToken))))
    | "e" => now(Move(Extreme(Right(ByToken))))
    | _ => []
    }
  | (D(key), platform, Shift(Up), Meta(Up), Ctrl(Up), Alt(Down)) =>
    switch (platform, key) {
    | (_, "ArrowLeft") when restricted =>
      now(MoveToBackpackTarget(Left(ByToken)))
    | (_, "ArrowRight") when restricted =>
      now(MoveToBackpackTarget(Right(ByToken)))
    | (IsMac, "ArrowLeft") => now(Move(Local(Left(ByToken))))
    | (IsMac, "ArrowRight") => now(Move(Local(Right(ByToken))))
    | (_, "Alt") => [SetShowBackpackTargets(true), UpdateDoubleTap(None)]
    | (_, "ArrowUp") => now(MoveToBackpackTarget(Up))
    | (_, "ArrowDown") => now(MoveToBackpackTarget(Down))
    | _ => []
    }
  | _ => []
  };
};

let do_many = (evts): Virtual_dom.Vdom.Event.t => {
  Virtual_dom.Vdom.Event.(
    switch (evts) {
    | [] => Many([])
    | evts => Many([Prevent_default, Stop_propagation, ...evts])
    }
  );
};

let update_handler = (~inject, ~model, ~dir: Key.dir, evt) =>
  Key.mk(dir, evt)
  |> handle_key_event(~model)
  |> List.map(inject)
  |> do_many;

let handlers = (~inject, ~model: Model.t) =>
  Virtual_dom.Vdom.[
    Attr.on_keypress(_ => Event.Prevent_default),
    Attr.on_keyup(update_handler(~inject, ~model, ~dir=KeyUp)),
    Attr.on_keydown(update_handler(~inject, ~model, ~dir=KeyDown)),
  ];
