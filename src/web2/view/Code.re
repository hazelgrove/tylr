open New;

let hole_decorations = (~font_metrics: FontMetrics.t, )

let view = (~font_metrics: FontMetrics.t, edit_state: EditState.re) => {
  let e = failwith("zip up edit_state");
  let text = Node.span(
    [Attr.classes(["code-text"])],
    [Text.space(Text.Exp.view(e))],
  );
  let mode_decorations = mode_decorations(~font_metrics, edit_state);
  let hole_decorations = hole_decorations(~font_metrics, edit_state);
  Node.div(
    [Attr.id("code")],
    List.concat([[text], mode_decorations, hole_decorations]),
  );
}