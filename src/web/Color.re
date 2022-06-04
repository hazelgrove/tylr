open Core;

[@deriving show]
type t =
  | Any
  | Typ
  | Pat
  | Exp
  | Selected;

let of_sort =
  fun
  | Sort.Any => Any
  | Pat => Pat
  | Exp => Exp;

let to_string =
  fun
  | Any => "Any"
  | Typ => "Typ"
  | Pat => "Pat"
  | Exp => "Exp"
  // TODO fix
  | Selected => "unsorted";
