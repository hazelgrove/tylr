[@deriving sexp]
type t =
  | Pat
  | Exp;

let to_string =
  fun
  | Pat => "Pat"
  | Exp => "Exp";
