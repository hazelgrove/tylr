open Sexplib.Std;
open Virtual_dom.Vdom;
open Util;
open Core;

[@deriving sexp]
type t =
  | Pointing
  | Selecting(Direction.t, Selection.t)
  | Restructuring({
      at_restructurable_selection: bool,
      backpack: Restructuring.Backpack.t,
      view: [@sexp.opaque] (Node.t, list(Node.t)),
    });
