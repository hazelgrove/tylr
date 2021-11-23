open Util;

[@deriving sexp]
type t =
  | Paren_l
  | Paren_r;

let tip = (d: Direction.t, t: t) =>
  switch (d, t) {
  | (Left, Paren_l) => (Tip.Convex, Sort.Pat)
  | (Right, Paren_l) => (Concave, Pat)
  | (Left, Paren_r) => (Concave, Pat)
  | (Right, Paren_r) => (Convex, Pat)
  };

let is_end = (~strict as _, d: Direction.t, t: t) =>
  switch (d, t) {
  | (Left, Paren_l)
  | (Right, Paren_r) => true
  | _ => false
  };

let is_next = (d: Direction.t, t1, t2) =>
  switch (d, t1, t2) {
  | (Right, Paren_l, Paren_r)
  | (Left, Paren_r, Paren_l) => true
  | _ => false
  };
