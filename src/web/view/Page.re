open Virtual_dom.Vdom;

let view = (~inject as _, _: Model.t) =>
  Node.div(
    [Attr.classes(["page"])],
    [Node.span([Attr.id("font-specimen")], [Node.text("X")])],
  );
