type t =
  | Paren_l
  | Paren_r
  | Let_eq(HPat.t)
  | Let_in;