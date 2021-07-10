open Sexplib.Std;
open Util;

[@deriving sexp]
type s = list(t)
and t =
  | OpHole
  | Num(int)
  | Var(Var.t)
  | Paren(s)
  | Lam(Tile_pat.s)
  | Let(Tile_pat.s, s)
  | BinHole
  | Plus
  | Times
  | Prod
  | Ap
  | Cond(s);

let precedence: t => int =
  fun
  | OpHole
  | Num(_)
  | Var(_)
  | Paren(_) => 0
  | Ap => 1
  | Times => 2
  | Plus => 3
  | Prod => 4
  | Cond(_) => 5
  | Lam(_) => 6
  | Let(_) => 7
  | BinHole => 8;

let associativity =
  [
    (1, Associativity.Left),
    (2, Left),
    (3, Left),
    (4, Right),
    (5, Right),
    (8, Left),
  ]
  |> List.to_seq
  |> IntMap.of_seq;

let is_hole =
  fun
  | OpHole
  | BinHole => true
  | _ => false;

let tip = (d: Direction.t, t: t) => {
  let shape =
    switch (d, t) {
    | (_, OpHole | Num(_) | Var(_) | Paren(_))
    | (Left, Lam(_) | Let(_)) => Tip.Convex
    | (Right, Lam(_) | Let(_))
    | (_, BinHole | Plus | Times | Prod | Ap | Cond(_)) => Concave
    };
  (shape, Sort.Exp);
};
