open Cor;

[@deriving sexp]
type t =
  | Pointing
  | Selecting
  | Restructuring(Restructuring.Backpack.t);
