open Util;

[@deriving sexp]
type t('op, 'pre, 'post, 'bin) =
  | Op('op)
  | Pre('pre)
  | Post('post)
  | Bin('bin);

let get_op: t('op, _, _, _) => 'op =
  fun
  | Op(op) => op
  | _ => raise(Invalid_argument("Tile.get_op"));
let get_pre: t(_, 'pre, _, _) => 'pre =
  fun
  | Pre(pre) => pre
  | _ => raise(Invalid_argument("Tile.get_pre"));
let get_post: t(_, _, 'post, _) => 'post =
  fun
  | Post(post) => post
  | _ => raise(Invalid_argument("Tile.get_post"));
let get_bin: t(_, _, _, 'bin) => 'bin =
  fun
  | Bin(bin) => bin
  | _ => raise(Invalid_argument("Tile.get_bin"));

let is_op =
  fun
  | Op(_) => true
  | _ => false;
let is_bin =
  fun
  | Bin(_) => true
  | _ => false;

let get =
    (
      get_op: 'op => 'a,
      get_pre: 'pre => 'a,
      get_post: 'post => 'a,
      get_bin: 'bin => 'a,
    )
    : (t('op, 'pre, 'post, 'bin) => 'a) =>
  fun
  | Op(op) => get_op(op)
  | Pre(pre) => get_pre(pre)
  | Post(post) => get_post(post)
  | Bin(bin) => get_bin(bin);

module type S = {
  module Term: Term.S;

  type nonrec t = t(Term.op, Term.pre, Term.post, Term.bin);

  let precedence: t => int;
  let associativity: IntMap.t(Associativity.t);

  let is_convex: (Direction.t, t) => bool;
};
