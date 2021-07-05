[@deriving sexp]
type t =
  | Pat
  | Exp;

let to_string =
  fun
  | Pat => "Pat"
  | Exp => "Exp";

let to_proper_string =
  fun
  | Pat => "pattern"
  | Exp => "expression";
