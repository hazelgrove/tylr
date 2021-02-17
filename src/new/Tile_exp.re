// Tile.S with module Tm := Term_exp

open Util;
open Term_exp;

type t = Tile.t(op, pre, post, bin);

let precedence: t => int =
  Tile.get(
    _ => 0,
    fun
    | Lam(_) => 4
    | Let(_) => 5,
    fun
    | Ap(_) => 1,
    fun
    | Plus => 3
    | BinHole => 2,
  );

let associativity =
  [(2, Associativity.Left), (3, Left)] |> List.to_seq |> IntMap.of_seq;

let is_convex = failwith("todo");
