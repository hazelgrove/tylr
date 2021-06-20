open Cor;

[@deriving sexp]
type t =
  | Pointing
  | Selecting
  | Restructuring(Selection.t);
