type t('operand, 'preop, 'postop, 'binop) =
  | Operand('operand)
  | PreOp('preop)
  | PostOp('postop)
  | BinOp('binop);

let map =
    (
      ~operand: 'operand => 'operand,
      ~preop: 'preop => 'preop,
      ~postop: 'postop => 'postop,
      ~binop: 'binop => 'binop,
      tile: t('operand, 'preop, 'postop, 'binop),
    )
    : t('operand, 'preop, 'postop, 'binop) =>
  switch (tile) {
  | Operand(t) => Operand(operand(t))
  | PreOp(t) => PreOp(preop(t))
  | PostOp(t) => PostOp(postop(t))
  | BinOp(t) => BinOp(binop(t))
  };
