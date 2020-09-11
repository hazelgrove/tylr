type t('operand, 'preop, 'postop, 'binop) =
  | Operand('operand)
  | PreOp('preop)
  | PostOp('postop)
  | BinOp('binop);

let get_operand: t('operand, _, _, _) => 'operand =
  fun
  | Operand(operand) => operand
  | _ => raise(Invalid_argument("Tile.get_operand"));
let get_preop: t(_, 'preop, _, _) => 'preop =
  fun
  | PreOp(preop) => preop
  | _ => raise(Invalid_argument("Tile.get_preop"));
let get_postop: t(_, _, 'postop, _) => 'postop =
  fun
  | PostOp(postop) => postop
  | _ => raise(Invalid_argument("Tile.get_postop"));
let get_binop: t(_, _, _, 'binop) => 'binop =
  fun
  | BinOp(binop) => binop
  | _ => raise(Invalid_argument("Tile.get_binop"));

let map =
    (
      f_operand: 'operand => 'a,
      f_preop: 'preop => 'a,
      f_postop: 'postop => 'a,
      f_binop: 'binop => 'a,
    )
    : (t('operand, 'preop, 'postop, 'binop) => 'a) =>
  fun
  | Operand(operand) => f_operand(operand)
  | PreOp(preop) => f_preop(preop)
  | PostOp(postop) => f_postop(postop)
  | BinOp(binop) => f_binop(binop);
