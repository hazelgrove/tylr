open Virtual_dom.Vdom;

let view = (~inject as _, model: Model.t) =>
  Node.div(
    [Attr.classes(["page"])],
    [
      FontSpecimen.view(),
      Code.view(~font_metrics=model.font_metrics, model.edit_state),
    ],
  );
