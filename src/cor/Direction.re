[@deriving sexp]
type t =
  | Left
  | Right;

let toggle =
  fun
  | Left => Right
  | Right => Left;

let sign =
  fun
  | Left => (-1)
  | Right => 1;
