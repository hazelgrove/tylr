[@deriving sexp]
type t =
  | Pointing(Selection.frame)
  | Selecting(Selection.t, Selection.frame)
  | Restructuring(Selection.t, Selection.frame);
