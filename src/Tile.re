type t('operand, 'preop, 'postop, 'binop) =
  | Operand('operand)
  | PreOp('preop)
  | PostOp('postop)
  | BinOp('binop);
