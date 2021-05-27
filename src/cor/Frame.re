type t =
  | Pat(Frame_pat.t)
  | Exp(Frame_exp.t);

let sort =
  fun
  | Pat(_) => Sort.Pat
  | Exp(_) => Exp;
