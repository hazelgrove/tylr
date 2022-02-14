open Sexplib.Std;

[@deriving sexp]
type t = list((Ancestor.t, Siblings.t));

let empty = [];

let rec sort: t => Sort.t =
  fun
  | [] => Sort.Exp
  | [(Intact(ancestor), _), ..._] => Ancestor.Intact.sort(ancestor)
  | [(Pieces(_), _), ...ancestors] => sort(ancestors);
