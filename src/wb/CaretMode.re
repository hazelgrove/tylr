open Virtual_dom.Vdom;
open Util;
open Cor;

[@deriving sexp]
type t =
  | Pointing
  | Selecting
  | Restructuring({
      backpack: Restructuring.Backpack.t,
      view: [@sexp.opaque] (Node.t, ListFrame.t(Node.t)),
    });
