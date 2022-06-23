open Core;

let is_printable = s => Re.Str.(string_match(regexp("^[ -~]$"), s, 0));

let is_digit = s => Re.Str.(string_match(regexp("^[0-9]$"), s, 0));

let handle_key_event =
    ({key, _} as evt: JsUtil.key_event, ~model): list(Update.t) => {
  let zipper = Model.get_zipper(model);
  let held = modifier => List.mem(modifier, evt.modifiers);
  let now = a => [Update.PerformAction(a), Update.UpdateDoubleTap(None)];
  //TODO(andrew): think harder about when/where to save
  let now_save_u = u => Update.[u, Save, UpdateDoubleTap(None)];
  let now_save = a => now_save_u(PerformAction(a));
  switch (evt.dir) {
  | KeyUp =>
    switch (key) {
    | "Shift" => [] // NOTE: don't change doubletap
    | "Alt" => [SetShowBackpackTargets(false)]
    | _ => [UpdateDoubleTap(None)]
    }
  | KeyDown =>
    if (!held(Ctrl) && !held(Alt) && !held(Meta)) {
      switch (key) {
      | "Home" => now(Move(Extreme(Left(ByToken))))
      | "End" => now(Move(Extreme(Right(ByToken))))
      | "ArrowLeft" when held(Shift) => now(Select(Local(Left(ByToken))))
      | "ArrowRight" when held(Shift) =>
        now(Select(Local(Right(ByToken))))
      | "ArrowUp" when held(Shift) => now(Select(Local(Up)))
      | "ArrowDown" when held(Shift) => now(Select(Local(Down)))
      | "Escape" => now(Unselect)
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
      | "F2" =>
        zipper |> Zipper.show |> print_endline;
        [];
      | "F4" => [Save]
      | "F6" => [Load]
      | "F8" => [LoadDefault, Save]
      | "Shift" =>
        let cur_ts = JsUtil.timestamp();
        switch (model.double_tap) {
        | None => [UpdateDoubleTap(Some(cur_ts))]
        | Some(past_ts) =>
          if (cur_ts -. past_ts < 400.) {
            [UpdateDoubleTap(None), PerformAction(RotateBackpack)];
          } else {
            [UpdateDoubleTap(Some(cur_ts))];
          }
        };
      | "Enter" =>
        //NOTE(andrew): using funky char to avoid weird regexp issues with using \n
        now_save(Insert(Whitespace.linebreak))
      | _ when Form.is_valid_char(key) && String.length(key) == 1 =>
        //TODO(andrew): length==1 is hack to prevent things like F5 which are now valid tokens
        // and also weird unicode shit i guess
        now_save(Insert(key))
      | _ when is_printable(key) => [FailedInput(Unrecognized)]
      | _ => []
      };
    } else if (!evt.is_mac
               && held(Ctrl)
               && !held(Alt)
               && !held(Meta)
               || evt.is_mac
               && held(Meta)
               && !held(Alt)
               && !held(Ctrl)) {
      switch (key) {
      // OS-agnostic
      | "z"
      | "Z" => now_save_u(held(Shift) ? Redo : Undo)
      | "x" => now(Pick_up)
      | "v" => now(Put_down)
      | "a" => now(Move(Extreme(Up))) @ now(Select(Extreme(Down)))
      | _ when is_digit(key) => [SwitchEditor(int_of_string(key))]
      // mac-specific
      | "ArrowLeft" when evt.is_mac && !held(Shift) =>
        now(Move(Extreme(Left(ByToken))))
      | "ArrowRight" when evt.is_mac && !held(Shift) =>
        now(Move(Extreme(Right(ByToken))))
      | "ArrowUp" when evt.is_mac && !held(Shift) => now(Move(Extreme(Up)))
      | "ArrowDown" when evt.is_mac && !held(Shift) =>
        now(Move(Extreme(Down)))
      | "ArrowLeft" when evt.is_mac && held(Shift) =>
        now(Select(Extreme(Left(ByToken))))
      | "ArrowRight" when evt.is_mac && held(Shift) =>
        now(Select(Extreme(Right(ByToken))))
      | "ArrowUp" when evt.is_mac && held(Shift) =>
        now(Select(Extreme(Up)))
      | "ArrowDown" when evt.is_mac && held(Shift) =>
        now(Select(Extreme(Down)))
      // pc-specific
      | "ArrowLeft" when !evt.is_mac && !held(Shift) =>
        now(Move(Local(Left(ByToken))))
      | "ArrowRight" when !evt.is_mac && !held(Shift) =>
        now(Move(Local(Right(ByToken))))
      | "Home" when !evt.is_mac && !held(Shift) => now(Move(Extreme(Up)))
      | "End" when !evt.is_mac && !held(Shift) => now(Move(Extreme(Down)))
      | "Home" when !evt.is_mac && held(Shift) => now(Select(Extreme(Up)))
      | "End" when !evt.is_mac && held(Shift) => now(Select(Extreme(Down)))
      | "ArrowLeft" when !evt.is_mac && held(Shift) =>
        now(Select(Local(Left(ByToken))))
      | "ArrowRight" when !evt.is_mac && held(Shift) =>
        now(Select(Local(Right(ByToken))))
      | "ArrowUp" when !evt.is_mac && held(Shift) => now(Select(Local(Up)))
      | "ArrowDown" when !evt.is_mac && held(Shift) =>
        now(Select(Local(Down)))
      | _ => []
      };
    } else if (evt.is_mac && held(Ctrl) && !held(Meta) && !held(Alt)) {
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
      | "ArrowLeft" when restricted =>
        now(MoveToBackpackTarget(Left(ByToken)))
      | "ArrowLeft" when evt.is_mac => now(Move(Local(Left(ByToken))))
      | "ArrowRight" when restricted =>
        now(MoveToBackpackTarget(Right(ByToken)))
      | "ArrowRight" when evt.is_mac => now(Move(Local(Right(ByToken))))
      | "ArrowUp" => now(MoveToBackpackTarget(Up))
      | "ArrowDown" => now(MoveToBackpackTarget(Down))
      | _ => []
      };
    } else {
      [];
    }
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

let update_handler = (~inject, ~model, ~dir: JsUtil.key_dir, evt) =>
  JsUtil.mk_key_event(dir, evt)
  |> handle_key_event(~model)
  |> List.map(inject)
  |> do_many;

let handlers = (~inject, ~model: Model.t) =>
  Virtual_dom.Vdom.[
    Attr.on_keypress(_ => Event.Prevent_default),
    Attr.on_keyup(update_handler(~inject, ~model, ~dir=KeyUp)),
    Attr.on_keydown(update_handler(~inject, ~model, ~dir=KeyDown)),
  ];
