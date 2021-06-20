open Sexplib.Std;
open Util;

module Tile = {
  [@deriving sexp]
  type s = list(t)
  and t = Tile.t(op, pre, post, bin)
  and op =
    | OpHole
    | Text(string)
    | Paren(s)
  and pre =
    | Lam(s)
    | Let(s, s)
  and post =
    | Ap(s)
    | Ann(s)
  and bin =
    | BinHole
    | Plus
    | Arrow
    | Cond(s)
    | Prod;
};

module Tessera = {
  [@deriving sexp]
  type t =
    | OpHole
    | BinHole
    | Text(string)
    | Lam(Tile.s)
    | Ann(Tile.s)
    | Plus
    | Arrow
    | Prod
    | Paren_l
    | Paren_r
    | Let_eq(Tile.s)
    | Let_in
    | Cond_then
    | Cond_else;

  module Shape = {
    [@deriving sexp]
    type t =
      | Text(string)
      | Paren_l
      | Paren_r
      | Lam
      | Let_eq
      | Let_in
      | Ann
      | Plus
      | Arrow
      | Prod;
  };

  let is_closing =
    fun
    | OpHole
    | BinHole
    | Text(_)
    | Lam(_)
    | Ann(_)
    | Plus
    | Prod
    | Arrow
    | Paren_l
    | Let_eq(_) => false
    | Paren_r
    | Let_in
    | Cond_then
    | Cond_else => true;

  let is_convex = (d: Direction.t) =>
    fun
    | OpHole
    | Text(_) => true
    | Lam(_)
    | Paren_l
    | Let_eq(_) => d == Left
    | Ann(_)
    | Paren_r => d == Right
    | BinHole
    | Prod
    | Plus
    | Arrow
    | Let_in
    | Cond_then
    | Cond_else => false;

  let has_child =
    fun
    | OpHole
    | Text(_)
    | Paren_l
    | Paren_r
    | BinHole
    | Plus
    | Arrow
    | Prod
    | Let_in
    | Cond_then
    | Cond_else => false
    | Lam(_)
    | Let_eq(_)
    | Ann(_) => true;

  let is_end_of_tile = (side: Direction.t) =>
    fun
    | OpHole
    | Text(_)
    | Lam(_)
    | Ann(_)
    | BinHole
    | Plus
    | Prod
    | Arrow => true
    | Paren_l
    | Let_eq(_)
    | Cond_then => side == Left
    | Paren_r
    | Let_in
    | Cond_else => side == Right;

  let is_next = (d: Direction.t, t, t') =>
    switch (d, t, t') {
    | (Right, Paren_l, Paren_r)
    | (Left, Paren_r, Paren_l)
    | (Right, Let_eq(_), Let_in)
    | (Left, Let_in, Let_eq(_)) => true
    | _ => false
    };
};
