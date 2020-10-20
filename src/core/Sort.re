type t =
  | Typ
  | Pat
  | Exp;

let to_string =
  fun
  | Typ => "Typ"
  | Pat => "Pat"
  | Exp => "Exp";
