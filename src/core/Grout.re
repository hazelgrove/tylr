[@deriving sexp]
type t =
  | Sep(Nibs.t)
  | Hole(Sort.t);
