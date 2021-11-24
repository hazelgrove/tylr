open Util;

[@deriving sexp]
type shape =
  | Convex
  | Concave;

[@deriving sexp]
type t = (shape, Sort.t);

let toggle_shape =
  fun
  | Convex => Concave
  | Concave => Convex;
let toggle: t => t = PairUtil.map_fst(toggle_shape);
