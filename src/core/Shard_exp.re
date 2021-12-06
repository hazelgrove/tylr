open Util;

[@deriving sexp]
type t = Identified.t(t')
and t' =
  | Paren_l
  | Paren_r
  | Ap_l
  | Ap_r
  | Lam_lam_open(Tile_pat.s)
  | Lam_lam
  | Lam_open
  | Lam_close
  | Let_let_eq(Tile_pat.s)
  | Let_let
  | Let_eq
  | Let_in
  | Cond_que
  | Cond_col;

let tip = (d: Direction.t, (_, t): t) =>
  switch (d, t) {
  | (Left, Paren_l | Lam_lam_open(_) | Lam_lam | Let_let | Let_let_eq(_))
  | (Right, Paren_r | Ap_r | Lam_close) => (Tip.Convex, Sort.Exp)
  | (Left, Paren_r | Ap_l | Ap_r | Lam_close | Let_in | Cond_que | Cond_col)
  | (
      Right,
      Paren_l | Ap_l | Lam_lam_open(_) | Lam_open | Let_let_eq(_) | Let_eq |
      Let_in |
      Cond_que |
      Cond_col,
    ) => (
      Concave,
      Exp,
    )
  | (Left, Lam_open | Let_eq)
  | (Right, Lam_lam | Let_let) => (Concave, Pat)
  };

let is_end = (~strict: bool, d: Direction.t, (_, t): t) =>
  switch (d, t) {
  | (
      Left,
      Paren_l | Ap_l | Lam_lam_open(_) | Lam_lam | Let_let | Let_let_eq(_) |
      Cond_que,
    )
  | (Right, Paren_r | Ap_r | Lam_close | Let_in | Cond_col) => true
  | (Right, Lam_open | Let_eq) => !strict
  | _ => false
  };

let is_next = (d: Direction.t, (_, t1), (_, t2)) =>
  switch (d) {
  | Left =>
    switch (t1, t2) {
    | (Paren_r, Paren_l)
    | (Ap_r, Ap_l)
    | (Lam_open, Lam_lam)
    | (Lam_close, Lam_open)
    | (Lam_close, Lam_lam_open(_))
    | (Let_eq, Let_let)
    | (Let_in, Let_eq)
    | (Let_in, Let_let_eq(_))
    | (Cond_col, Cond_que) => true
    | _ => false
    }
  | Right =>
    switch (t1, t2) {
    | (Paren_l, Paren_r)
    | (Ap_l, Ap_r)
    | (Lam_lam, Lam_open)
    | (Lam_open, Lam_close)
    | (Lam_lam_open(_), Lam_close)
    | (Let_let, Let_eq)
    | (Let_eq, Let_in)
    | (Let_let_eq(_), Let_in)
    | (Cond_que, Cond_col) => true
    | _ => false
    }
  };
