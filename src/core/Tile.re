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

let is_convex = (d: Direction.t, t: t(_)) =>
  switch (d, t) {
  | (_, Op(_))
  | (Left, Pre(_))
  | (Right, Post(_)) => true
  | _ => false
  };

module type S = {
  module Tm: Term.S;

  [@deriving sexp]
  type nonrec t = t(Tm.op, Tm.pre, Tm.post, Tm.bin);

  let precedence: t => int;
  let associativity: IntMap.t(Associativity.t);
};
