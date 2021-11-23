open Sexplib.Std;
open Util;

[@deriving sexp]
type s = list(t)
and t =
  | OpHole
  | Var(Var.t)
  | Paren(s)
  | BinHole
  | Prod;

let precedence: t => int =
  fun
  | OpHole
  | Var(_)
  | Paren(_) => 0
  | Prod => 1
  | BinHole => 2;

let associativity =
  [(1, Associativity.Right), (2, Left)] |> List.to_seq |> IntMap.of_seq;

let is_hole =
  fun
  | OpHole
  | BinHole => true
  | _ => false;

let is_leaf =
  fun
  | OpHole
  | Var(_)
  | BinHole
  | Prod => true
  | Paren(_) => false;

let tip = (d: Direction.t, t: t) => {
  let shape =
    switch (d, t) {
    | (_, OpHole | Var(_) | Paren(_)) => Tip.Convex
    | (_, BinHole | Prod) => Concave
    };
  (shape, Sort.Pat);
};
