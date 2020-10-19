open Virtual_dom.Vdom;

let view = (~inject as _, model: Model.t) =>
  Node.div(
    [Attr.id("page")],
    [
      FontSpecimen.view(),
      Node.div(
        [Attr.id("code-container")],
        [Code.view(~font_metrics=model.font_metrics, model.edit_state)],
      ),
    ],
  );
