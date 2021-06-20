open Util;

[@deriving sexp]
type t =
  | Pat(Token_pat.t)
  | Exp(Token_exp.t);

let get = (get_pat, get_exp) =>
  fun
  | Pat(t) => get_pat(t)
  | Exp(t) => get_exp(t);

let sort = get(_ => Sort.Pat, _ => Sort.Exp);

let tip = d => get(Token_pat.tip(d), Token_exp.tip(d));

let is_end = d => get(Token_pat.is_end(d), Token_exp.is_end(d));

let is_next = (d: Direction.t, t1: t, t2: t) =>
  switch (t1, t2) {
  | (Pat(t1), Pat(t2)) => Token_pat.is_next(d, t1, t2)
  | (Exp(t1), Exp(t2)) => Token_exp.is_next(d, t1, t2)
  | _ => false
  };
