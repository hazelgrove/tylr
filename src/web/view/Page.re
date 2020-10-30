open Js_of_ocaml;
open Virtual_dom.Vdom;

let key_handlers = (~inject: Update.t => Event.t) => {
  [
    Attr.on_keypress(_ => Event.Prevent_default),
    Attr.on_keydown(evt => {
      let key = Js.to_string(Js.Optdef.get(evt##.key, () => assert(false)));
      let action: option(Core.Action.t) =
        switch (key) {
        | "ArrowLeft" => Some(Move(Left))
        | "ArrowRight" => Some(Move(Right))
        | "Backspace" => Some(Delete(Left))
        | "Delete" => Some(Delete(Right))
        | "+" => Some(Construct(Plus))
        | "(" => Some(Construct(Paren))
        | _ => None
        };
      switch (action) {
      | None => Event.Many([])
      | Some(a) =>
        Event.(
          Many([
            Prevent_default,
            Stop_propagation,
            inject(PerformAction(a)),
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
          ...key_handlers(~inject),
        ],
        [Code.view(~font_metrics=model.font_metrics, model.edit_state)],
      ),
    ],
  );
