open Util;

/**
 * higher precedence means lower int representation
 */
[@deriving show]
type t = int;

let max: t = 0;

let fact = 1;
let ap = 2;
let mult = 3;
let plus = 4;
let prod = 5;
let eqs = 6;
let gt = 6;
let cond = 7;
let let_ = 8;
let fun_ = 9;
let min = 10;

let compare = (p1: t, p2: t): int =>
  (-1) * Int.compare((p1 :> int), (p2 :> int));
// let min = (p1: t, p2: t): t => max(p1, p2);

let associativity_map: IntMap.t(Direction.t) =
  [
    (mult, Direction.Left),
    (plus, Left),
    (prod, Right),
    (cond, Left),
    (eqs, Right),
    (gt, Right),
    (prod, Right),
  ]
  |> List.to_seq
  |> IntMap.of_seq;

let associativity = (p: t): option(Direction.t) =>
  IntMap.find_opt(p, associativity_map);
