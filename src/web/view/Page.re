module Vdom = Virtual_dom.Vdom;

let view = (~inject as _, model: Action.Exp.edit_state) =>
  switch (model) {
  | Normal({focus}) =>
    Vdom.(Node.div([Attr.classes(["page"])], [Code_Exp.view_z(focus)]))
  | _ => failwith("unimplemented")
  };
