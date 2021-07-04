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
  | Ap;

let precedence: t => int =
  fun
  | OpHole
  | Num(_)
  | Var(_)
  | Paren(_) => 0
  | Ap => 1
  | Plus => 2
  | Times => 3
  | Prod => 4
  | Lam(_) => 5
  | Let(_) => 6
  | BinHole => 7;

let associativity =
  [(1, Associativity.Left), (2, Left), (3, Left), (4, Right), (7, Left)]
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
    | (_, BinHole | Plus | Times | Prod | Ap) => Concave
    };
  (shape, Sort.Exp);
};
