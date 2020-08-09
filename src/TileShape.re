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
