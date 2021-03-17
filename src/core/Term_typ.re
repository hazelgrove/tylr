open Sexplib.Std;

[@deriving sexp]
type t = Term.t(op, pre, post, bin)
and op =
  | OpHole
  | Num
  | Bool
  | Paren(t)
and pre = unit // empty
and post = unit // empty
and bin =
  | BinHole
  | Arrow;

exception Void_pre;
exception Void_post;

let mk_op_hole = () => OpHole;
let mk_bin_hole = () => BinHole;

let is_op_hole =
  fun
  | OpHole => true
  | _ => false;
let is_bin_hole =
  fun
  | BinHole => true
  | _ => false;
