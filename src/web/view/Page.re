open Js_of_ocaml;
open Virtual_dom.Vdom;

let key_handlers = (~inject: Update.t => Event.t) => {
  let prevent_stop_inject = (u: Update.t) =>
    Event.Many([Event.Prevent_default, Event.Stop_propagation, inject(u)]);
  [
    Attr.on_keypress(_ => Event.Prevent_default),
    Attr.on_keydown(evt => {
      let key = Js.to_string(Js.Optdef.get(evt##.key, () => assert(false)));
      switch (key) {
      | "ArrowLeft" => prevent_stop_inject(PerformAction(Move(Left)))
      | "ArrowRight" => prevent_stop_inject(PerformAction(Move(Right)))
      | _ => Event.Prevent_default
      };
    }),
  ];
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
          ...key_handlers(~inject),
        ],
        [Code.view(~font_metrics=model.font_metrics, model.edit_state)],
      ),
    ],
  );
