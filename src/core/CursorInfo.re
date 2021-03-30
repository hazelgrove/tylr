[@deriving sexp]
type t =
  | Typ
  | Pat(TypeInfo_pat.t, Type.t)
  | Exp(TypeInfo_exp.t, Type.t);
