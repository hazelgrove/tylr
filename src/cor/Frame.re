type t =
  | Pat(Frame_pat.t)
  | Exp(Frame_exp.t);

let get = (get_pat, get_exp) =>
  fun
  | Pat(frame) => get_pat(frame)
  | Exp(frame) => get_exp(frame);

let get_pat =
  fun
  | Pat(frame) => Some(frame)
  | Exp(_) => None;
let get_exp =
  fun
  | Exp(frame) => Some(frame)
  | Pat(_) => None;

let sort =
  fun
  | Pat(_) => Sort.Pat
  | Exp(_) => Exp;
