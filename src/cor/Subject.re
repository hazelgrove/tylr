[@deriving sexp]
type t =
  | Pointing(Selection.frame)
  | Selecting(Direction.t, Selection.t, Selection.frame)
  | Restructuring(Selection.t, Selection.frame);
