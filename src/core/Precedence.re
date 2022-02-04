open Sexplib.Std;

/**
 * higher precedence means lower int representation
 */
[@deriving sexp]
type t = int;

// let of_sexp: Sexplib.Sexp.t => t = sexp => max_p;

let max_p: t = 0;

let fact = 1;
let ap = 2;
let mult = 3;
let plus = 4;
let prod = 5;
let cond = 6;
let let_ = 7;

let compare = (p1: t, p2: t): int =>
  (-1) * Int.compare((p1 :> int), (p2 :> int));
let min = (p1: t, p2: t): t => max(p1, p2);
