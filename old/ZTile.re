type t('zoperand, 'zpre_unop, 'zpost_unop, 'zbinop) =
  | ZOperand('zoperand)
  | ZPre('zpre_unop)
  | ZPost('zpost_unop)
  | ZBinOp('zbinop);

type s(
  'operand, 'pre_unop, 'post_unop, 'binop,
  'zoperand, 'zpre_unop, 'zpost_unop, 'zbinop,
) = ZList.t(
  t('zoperand, 'zpre_unop, 'zpost_unop, 'zbinop),
  Tile.t('operand, 'pre_unop, 'post_unop, 'binop)
);