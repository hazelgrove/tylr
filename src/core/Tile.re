[@deriving sexp]
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

let is_operand =
  fun
  | Operand(_) => true
  | _ => false;
let is_binop =
  fun
  | BinOp(_) => true
  | _ => false;

let get =
    (
      get_operand: 'operand => 'a,
      get_preop: 'preop => 'a,
      get_postop: 'postop => 'a,
      get_binop: 'binop => 'a,
    )
    : (t('operand, 'preop, 'postop, 'binop) => 'a) =>
  fun
  | Operand(operand) => get_operand(operand)
  | PreOp(preop) => get_preop(preop)
  | PostOp(postop) => get_postop(postop)
  | BinOp(binop) => get_binop(binop);

module type S = {
  let sort: Sort.t;

  [@deriving sexp]
  type operand;
  [@deriving sexp]
  type preop;
  [@deriving sexp]
  type postop;
  [@deriving sexp]
  type binop;
  [@deriving sexp]
  type nonrec t = t(operand, preop, postop, binop);
  [@deriving sexp]
  type s = list(t);

  let mk_operand_hole: unit => t;
  let mk_operator_hole: unit => t;

  let is_operand_hole: t => bool;
  let is_operator_hole: t => bool;

  let precedence: t => int;
  let associativity: Util.IntMap.t(Associativity.t);

  let get_open_children: t => list(s);
};
