open Virtual_dom.Vdom;

let view = () =>
  Node.span([Attr.id("logo-font-specimen")], [Node.text("X")]);
