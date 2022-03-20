[@deriving show]
type t = list((Ancestor.t, Siblings.t));

let empty = [];

let sort =
  fun
  | [] => Sort.root
  | [(a, _), ..._] => Ancestor.sort(a);
