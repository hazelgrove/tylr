module Vdom = Virtual_dom.Vdom;

let view = (~inject as _, _) => Vdom.Node.div([], []);
