open Virtual_dom.Vdom;
open Util;
open Core;

[@deriving sexp]
type t =
  | Pointing
  | Selecting(Direction.t)
  | Restructuring({
      backpack: Restructuring.Backpack.t,
      view: [@sexp.opaque] (Node.t, list(Node.t)),
    });