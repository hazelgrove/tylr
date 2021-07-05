open Util;

[@deriving sexp]
type t =
  | Pat(Shard_pat.t)
  | Exp(Shard_exp.t);

let get = (get_pat, get_exp) =>
  fun
  | Pat(t) => get_pat(t)
  | Exp(t) => get_exp(t);

let sort = get(_ => Sort.Pat, _ => Sort.Exp);

let tip = d => get(Shard_pat.tip(d), Shard_exp.tip(d));

let is_end = (~strict, d) =>
  get(Shard_pat.is_end(~strict, d), Shard_exp.is_end(~strict, d));

let is_next = (d: Direction.t, t1: t, t2: t) =>
  switch (t1, t2) {
  | (Pat(t1), Pat(t2)) => Shard_pat.is_next(d, t1, t2)
  | (Exp(t1), Exp(t2)) => Shard_exp.is_next(d, t1, t2)
  | _ => false
  };
