open Js_of_ocaml;
open Virtual_dom.Vdom;
open Core;

let key_handlers = (~inject: Update.t => Event.t, ~mode: EditState.Mode.t) => {
  [
    Attr.on_keypress(_ => Event.Prevent_default),
    Attr.on_keydown(evt => {
      let key = Js.to_string(Js.Optdef.get(evt##.key, () => assert(false)));
      let held_shift = Js.to_bool(evt##.shiftKey);
      let actions: list(Core.Action.t) =
        switch (key) {
        | "ArrowLeft"
        | "ArrowRight" =>
          let d: Direction.t = key == "ArrowLeft" ? Left : Right;
          switch (mode) {
          | Normal(_) => held_shift ? [Mark, Move(d)] : [Move(d)]
          | Selecting(_) => held_shift ? [Move(d)] : [Mark, Mark, Move(d)]
          | Restructuring(_) => [Move(d)]
          };
        | "Backspace" => [Delete(Left)]
        | "Delete" => [Delete(Right)]
        | "+" => [Construct(Plus)]
        | "(" => [Construct(Paren)]
        | _ => []
        };
      switch (actions) {
      | [] => Event.Many([])
      | [_, ..._] =>
        Event.(
          Many([
            Prevent_default,
            Stop_propagation,
            ...List.map(a => inject(PerformAction(a)), actions),
          ])
        )
      };
    }),
  ];
};

let focus_code = () => {
  JsUtil.get_elem_by_id("code-container")##focus;
};

let view = (~inject, model: Model.t) =>
  Node.div(
    [Attr.id("page")],
    [
      FontSpecimen.view(),
      Node.div(
        [
          Attr.id("code-container"),
          // necessary to make cell focusable
          Attr.create("tabindex", "0"),
          Attr.on_blur(_ => {
            focus_code();
            Event.Prevent_default;
          }),
          ...key_handlers(~inject, ~mode=fst(model.edit_state)),
        ],
        [Code.view(~font_metrics=model.font_metrics, model.edit_state)],
      ),
    ],
  );
