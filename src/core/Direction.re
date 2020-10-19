[@deriving sexp]
type t =
  | Left
  | Right;

let toggle =
  fun
  | Left => Right
  | Right => Left;
