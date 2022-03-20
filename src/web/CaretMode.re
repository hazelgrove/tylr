// // open Virtual_dom.Vdom;
// open Util;
// open Core;
// [@deriving show]
// type t =
//   | Pointing
//   | Selecting(Direction.t, Segment.t)
//   | Restructuring({
//       at_restructurable_selection: bool,
//       backpack: Restructuring.Backpack.t,
//       view: [@sexp.opaque] (Node.t, list(Node.t)),
//     });
type t = unit;
