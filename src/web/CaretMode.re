open Core;

[@deriving sexp]
type t =
  | Pointing
  | Selecting
  | Restructuring({
      selection: Selection.t,
      view: [@sexp.opaque] Virtual_dom.Vdom.Node.t,
    });
