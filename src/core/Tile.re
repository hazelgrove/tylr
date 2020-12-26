[@deriving sexp]
type t('op, 'pre, 'post, 'bin) =
  | Op('op)
  | Pre('pre)
  | Post('post)
  | Bin('bin);

let get_operand: t('op, _, _, _) => 'op =
  fun
  | Op(op) => op
  | _ => raise(Invalid_argument("Tile.get_operand"));
let get_preop: t(_, 'pre, _, _) => 'pre =
  fun
  | Pre(pre) => pre
  | _ => raise(Invalid_argument("Tile.get_preop"));
let get_postop: t(_, _, 'post, _) => 'post =
  fun
  | Post(post) => post
  | _ => raise(Invalid_argument("Tile.get_postop"));
let get_binop: t(_, _, _, 'bin) => 'bin =
  fun
  | Bin(bin) => bin
  | _ => raise(Invalid_argument("Tile.get_binop"));

let is_operand =
  fun
  | Op(_) => true
  | _ => false;
let is_binop =
  fun
  | Bin(_) => true
  | _ => false;

let get =
    (
      get_operand: 'op => 'a,
      get_preop: 'pre => 'a,
      get_postop: 'post => 'a,
      get_binop: 'bin => 'a,
    )
    : (t('op, 'pre, 'post, 'bin) => 'a) =>
  fun
  | Op(op) => get_operand(op)
  | Pre(pre) => get_preop(pre)
  | Post(post) => get_postop(post)
  | Bin(bin) => get_binop(bin);

module type S = {
  let sort: Sort.t;

  [@deriving sexp]
  type op;
  [@deriving sexp]
  type pre;
  [@deriving sexp]
  type post;
  [@deriving sexp]
  type bin;
  [@deriving sexp]
  type nonrec t = t(op, pre, post, bin);
  [@deriving sexp]
  type s = list(t);

  let mk_op_hole: unit => t;
  let mk_bin_hole: unit => t;

  let is_op_hole: t => bool;
  let is_bin_hole: t => bool;

  let precedence: t => int;
  let associativity: Util.IntMap.t(Associativity.t);

  let get_open_children: t => list(s);
};
