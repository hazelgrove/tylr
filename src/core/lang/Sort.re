open Util;

[@deriving show]
type t =
  | Pat
  | Exp;

let all = [Pat, Exp];

let root = Exp;

let to_string =
  fun
  | Pat => "Pat"
  | Exp => "Exp";

let to_proper_string =
  fun
  | Pat => "pattern"
  | Exp => "expression";

module Stack = {
  type sort = t;
  type t = list(sort);

  let push = List.cons;

  let pop = ListUtil.split_first_opt;
};
