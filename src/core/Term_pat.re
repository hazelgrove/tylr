open Sexplib.Std;

[@deriving sexp]
type t = Term.t(op, pre, post, bin)
and op =
  | OpHole
  | Var(Var.t)
  | Paren(t)
and pre = unit // empty
and post =
  | Ann(Term_typ.t)
and bin =
  | BinHole
  | Prod;

exception Void_pre;

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
