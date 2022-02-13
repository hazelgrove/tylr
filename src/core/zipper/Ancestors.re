open Sexplib.Std;

[@deriving sexp]
type t = list((Ancestor.t, Siblings.t));

let empty = [];

let sort: t => Sort.t =
  fun
  | [] => Sort.Exp
  | [(ancestor, _), ..._] => Ancestor.sort(ancestor);
