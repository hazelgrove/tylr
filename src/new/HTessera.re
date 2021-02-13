open Util;

type t =
  | OpHole
  | BinHole
  | Text(string)
  | Lam(HTile.s)
  | Ann(HTile.s)
  | Plus
  | Arrow
  | Paren_l
  | Paren_r
  | Let_eq(/* pattern */ HTile.s)
  | Let_in;

// TODO review and consider making
// HTile and HTessera mutually recursive
let mk_tile: AltList.t(t, HTile.s) => option(HTile.t) =
  fun
  | A(Paren_l, Some(B(body, A(Paren_r, None)))) => Some(Op(Paren(body)))
  | A(Let_eq(p), Some(B(def, A(Let_in, None)))) =>
    Some(Pre(Let(p, def)))
  | _ => None;

module Shape = {
  type t =
    | Text(string)
    | Paren_l
    | Paren_r
    | Lam
    | Let_eq
    | Let_in
    | Ann
    | Plus
    | Arrow;
};
