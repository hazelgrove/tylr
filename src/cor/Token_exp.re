open Util;

[@deriving sexp]
type t =
  | Paren_l
  | Paren_r
  | Lam_lam
  | Lam_dot
  | Let_let
  | Let_eq
  | Let_in;

let tip = (d: Direction.t, t: t) =>
  switch (d, t) {
  | (Left, Paren_l | Lam_lam | Let_let)
  | (Right, Paren_r) => (Tip.Convex, Sort.Exp)
  | (Left, Paren_r | Let_in)
  | (Right, Paren_l | Lam_dot | Let_eq | Let_in) => (Concave, Exp)
  | (Left, Lam_dot | Let_eq)
  | (Right, Lam_lam | Let_let) => (Concave, Pat)
  };

let is_end = (d: Direction.t, t: t) =>
  switch (d, t) {
  | (Left, Paren_l | Lam_lam | Let_let)
  | (Right, Paren_r | Lam_dot | Let_in) => true
  | _ => false
  };

let is_next = (d: Direction.t, t1, t2) =>
  switch (d) {
  | Left =>
    switch (t1, t2) {
    | (Paren_r, Paren_l)
    | (Lam_dot, Lam_lam)
    | (Let_eq, Let_let)
    | (Let_in, Let_eq) => true
    | _ => false
    }
  | Right =>
    switch (t1, t2) {
    | (Paren_l, Paren_r)
    | (Lam_lam, Lam_dot)
    | (Let_let, Let_eq)
    | (Let_eq, Let_in) => true
    | _ => false
    }
  };
