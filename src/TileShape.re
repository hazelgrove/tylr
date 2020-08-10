type t('term) =
  | Operand('term)
  | PreOp('term => 'term, Precedence.t)
  | PostOp('term => 'term, Precedence.t)
  | BinOp(('term, 'term) => 'term, Precedence.t, Associativity.t);

let precedence =
  fun
  | Operand(_) => 0
  | PreOp(_, p)
  | PostOp(_, p)
  | BinOp(_, p, _) => p;

let flip =
  fun
  | (Operand(_) | BinOp(_)) as shape => shape
  | PreOp(mk, p) => PostOp(mk, p)
  | PostOp(mk, p) => PreOp(mk, p);

type hole_shape =
  | Operand
  | Operator;

let keystone_shape =
    (s1: option(t('term)), s2: option(t('term))): option(hole_shape) =>
  switch (s1, Option.map(flip, s2)) {
  | (None, None) => Some(Operand)
  | (Some(s), None)
  | (None, Some(s)) =>
    switch (s) {
    | Operand(_)
    | PostOp(_) => None
    | PreOp(_)
    | BinOp(_) => Some(Operand)
    }
  | (Some(s1), Some(s2)) =>
    switch (s1, s2) {
    | (Operand(_) | PostOp(_), Operand(_) | PostOp(_)) => Some(Operator)
    | (PreOp(_) | BinOp(_), PreOp(_) | BinOp(_)) => Some(Operand)
    | _ => None
    }
  };
