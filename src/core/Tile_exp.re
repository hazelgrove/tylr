open Sexplib.Std;
open Util;

[@deriving sexp]
type s = list(t)
and t =
  | OpHole
  | Num(int)
  | Var(Var.t)
  | Paren(s)
  | Lam(Tile_pat.s, s)
  | Let(Tile_pat.s, s)
  | Ap(s)
  | Fact
  | BinHole
  | Plus
  | Minus
  | Times
  | Div
  | Prod
  | Cond(s);

let precedence: t => int =
  fun
  | OpHole
  | Num(_)
  | Var(_)
  | Paren(_)
  | Lam(_) => 0
  | Fact => 1
  | Ap(_) => 2
  | Times
  | Div => 3
  | Plus
  | Minus => 4
  | BinHole => 5
  | Prod => 6
  | Cond(_) => 7
  | Let(_) => 9;

let associativity =
  [(3, Associativity.Left), (4, Left), (5, Right), (6, Right), (7, Left)]
  |> List.to_seq
  |> IntMap.of_seq;

let is_hole =
  fun
  | OpHole
  | BinHole => true
  | _ => false;

let is_leaf =
  fun
  | OpHole
  | Num(_)
  | Var(_)
  | Fact
  | BinHole
  | Plus
  | Minus
  | Times
  | Div
  | Prod => true
  | Paren(_)
  | Lam(_)
  | Let(_)
  | Ap(_)
  | Cond(_) => false;

let tip = (d: Direction.t, t: t) => {
  let shape =
    switch (d, t) {
    | (_, OpHole | Num(_) | Var(_) | Paren(_))
    | (Left, Lam(_) | Let(_))
    | (Right, Fact | Lam(_) | Ap(_)) => Tip.Convex
    | (_, BinHole | Plus | Minus | Times | Div | Prod | Cond(_))
    | (Right, Let(_))
    | (Left, Fact | Ap(_)) => Concave
    };
  (shape, Sort.Exp);
};
