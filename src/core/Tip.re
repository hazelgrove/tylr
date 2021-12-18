open Util;

[@deriving sexp]
type t =
  | Convex
  | Concave;

let toggle =
  fun
  | Convex => Concave
  | Concave => Convex;

