open Js_of_ocaml;
open Virtual_dom.Vdom;
open Core;

let key_handlers = (~inject: Update.t => Event.t, ~mode: EditState.Mode.t) => {
  [
    Attr.on_keypress(_ => Event.Prevent_default),
    Attr.on_keydown(evt => {
      let key = Js.to_string(Js.Optdef.get(evt##.key, () => assert(false)));
      let held_shift = Js.to_bool(evt##.shiftKey);
      let p = a => Update.PerformAction(a);
      let updates: list(Update.t) =
        switch (key) {
        | "ArrowLeft"
        | "ArrowRight" =>
          let d: Direction.t = key == "ArrowLeft" ? Left : Right;
          switch (mode) {
          | Normal(_) =>
            held_shift ? [p(Mark), p(Move(d))] : [p(Move(d))]
          | Selecting(_) =>
            held_shift ? [p(Move(d))] : [Escape, p(Move(d))]
          | Restructuring(_) => [p(Move(d))]
          };
        | "Backspace" => [p(Delete(Left))]
        | "Delete" => [p(Delete(Right))]
        | "+" => [p(Construct(Plus))]
        | "(" => [p(Construct(Paren))]
        | "Escape" => [Escape]
        | _ => []
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
