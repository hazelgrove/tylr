[@deriving sexp]
type t =
  | Typ
  | Pat(TypeInfo_pat.t)
  | Exp(TypeInfo_exp.t);
