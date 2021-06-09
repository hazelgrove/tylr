open Cor;

type t =
  | Pointing
  | Selecting
  | Restructuring(Selection.t);
