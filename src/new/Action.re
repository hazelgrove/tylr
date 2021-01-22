open Sexplib.Std;

[@deriving sexp]
type tile_shape =
  | Text(string)
  | Paren
  | Lam
  | Let
  | Ap
  | Ann
  | Plus
  | Arrow;

[@deriving sexp]
type t =
  | Mark
  | Move(Direction.t)
  | Delete(Direction.t)
  | Construct(tile_shape);
