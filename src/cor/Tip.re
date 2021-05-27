open Util;

type shape =
  | Convex
  | Concave;

type t = (shape, Sort.t);

let toggle_shape =
  fun
  | Convex => Concave
  | Concave => Convex;
let toggle: t => t = PairUtil.map_fst(toggle_shape);
