open Core;

[@deriving show]
type t =
  | Typ
  | Pat
  | Exp
  | Selected;

let of_sort =
  fun
  | Sort.Pat => Pat
  | Exp => Exp;

let to_string =
  fun
  | Typ => "Typ"
  | Pat => "Pat"
  | Exp => "Exp"
  // TODO fix
  | Selected => "unsorted";
