module Vdom = Virtual_dom.Vdom;

let view = (~inject, model) =>
  Vdom.(Node.div([Attr.classes(["page"])], [Code.view(~inject, model)]));
