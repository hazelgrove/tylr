open Util;

[@deriving sexp]
type t = Action_make.t;

let perform = (a: t, zipper: Zipper.t): option(Zipper.t) =>
  switch (zipper) {
  | Typ(zipper) => Action_typ.perform(a, zipper)
  | Pat(zipper) => Action_pat.perform(a, zipper)
  | Exp(zipper) => Action_exp.perform(a, zipper)
  };
