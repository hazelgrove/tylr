open Js_of_ocaml;
open Virtual_dom.Vdom;
open Core;

let is_var = s => Re.Str.string_match(Re.Str.regexp("[a-z]"), s, 0);

let key_handlers =
    (
      ~inject: Update.t => Event.t,
      ~edit_state as (mode, zipper): EditState.t,
    ) => {
  [
    Attr.on_keypress(_ => Event.Prevent_default),
    Attr.on_keydown(evt => {
      let key = Js.to_string(Js.Optdef.get(evt##.key, () => assert(false)));
      let no_ctrl_alt_meta = JsUtil.no_ctrl_alt_meta(evt);
      let held_shift = JsUtil.held_shift(evt);
      let p = a => Update.PerformAction(a);
      let updates: list(Update.t) =
        if (!no_ctrl_alt_meta) {
          [];
        } else {
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
          | "\\" => [p(Construct(Lam))]
          | "=" => [p(Construct(Let))]
          | ":" => [p(Construct(Ann))]
          | "Escape" => [Escape]
          | "Enter" =>
            switch (mode) {
            | Normal(_) => []
            | Selecting(_) => [Escape]
            | Restructuring(_) => [p(Mark)]
            }
          | _ =>
            switch (zipper) {
            | `Typ(_) =>
              if (key == "n") {
                [p(Construct(Num))];
              } else if (key == "b") {
                [p(Construct(Bool))];
              } else {
                [];
              }
            | `Pat(_)
            | `Exp(_) => is_var(key) ? [p(Construct(Var(key)))] : []
            }
          };
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
      NodeUtil.svg(
        [],
        [
          Decoration.EmptyHole.inset_shadow_filter,
          Decoration.EmptyHole.thin_inset_shadow_filter,
        ],
      ),
      Node.div(
        [
          Attr.id("code-container"),
          // necessary to make cell focusable
          Attr.create("tabindex", "0"),
          Attr.on_blur(_ => {
            focus_code();
            Event.Prevent_default;
          }),
          ...key_handlers(~inject, ~edit_state=model.edit_state),
        ],
        [Code.view(~font_metrics=model.font_metrics, model.edit_state)],
      ),
    ],
  );
