type t('operand, 'preop, 'postop, 'binop) =
  | Operand('operand)
  | PreOp('preop)
  | PostOp('postop)
  | BinOp('binop);

let get_operand =
  fun
  | Operand(operand) => operand
  | _ => raise(Invalid_argument("Tile.get_operand"));
let get_preop =
  fun
  | PreOp(preop) => preop
  | _ => raise(Invalid_argument("Tile.get_preop"));
let get_postop =
  fun
  | PostOp(postop) => postop
  | _ => raise(Invalid_argument("Tile.get_postop"));
let get_binop =
  fun
  | BinOp(binop) => binop
  | _ => raise(Invalid_argument("Tile.get_binop"));

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
