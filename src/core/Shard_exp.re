open Util;

[@deriving sexp]
type t =
  | Paren_l
  | Paren_r
  | Ap_l
  | Ap_r
  | Lam_lam
  | Lam_dot
  | Let_let_eq(Tile_pat.s)
  | Let_let
  | Let_eq
  | Let_in
  | Cond_que
  | Cond_col;

let tip = (d: Direction.t, t: t) =>
  switch (d, t) {
  | (Left, Paren_l | Lam_lam | Let_let | Let_let_eq(_))
  | (Right, Paren_r | Ap_r) => (Tip.Convex, Sort.Exp)
  | (Left, Paren_r | Ap_l | Ap_r | Let_in | Cond_que | Cond_col)
  | (
      Right,
      Paren_l | Ap_l | Lam_dot | Let_let_eq(_) | Let_eq | Let_in | Cond_que |
      Cond_col,
    ) => (
      Concave,
      Exp,
    )
  | (Left, Lam_dot | Let_eq)
  | (Right, Lam_lam | Let_let) => (Concave, Pat)
  };

let is_end = (~strict: bool, d: Direction.t, t: t) =>
  switch (d, t) {
  | (Left, Paren_l | Ap_l | Lam_lam | Let_let | Let_let_eq(_) | Cond_que)
  | (Right, Paren_r | Ap_r | Let_in | Cond_col) => true
  | (Right, Lam_dot | Let_eq) => !strict
  | _ => false
  };

let is_next = (d: Direction.t, t1, t2) =>
  switch (d) {
  | Left =>
    switch (t1, t2) {
    | (Paren_r, Paren_l)
    | (Ap_r, Ap_l)
    | (Lam_dot, Lam_lam)
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
    | (Lam_lam, Lam_dot)
    | (Let_let, Let_eq)
    | (Let_eq, Let_in)
    | (Let_let_eq(_), Let_in)
    | (Cond_que, Cond_col) => true
    | _ => false
    }
  };
