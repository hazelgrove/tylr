// open Util;

[@deriving sexp]
type t = Action_make.t;

let perform = (a: t, zipper: EditState.t): option(EditState.t) =>
  switch (zipper) {
  /*
   | Pat(zipper) => Action_pat.perform(a, zipper)
   */
  | Typ(zipper) => Action_typ.perform(a, zipper)
  | Exp(zipper) => Action_exp.perform(a, zipper)
  | _ => failwith("todo")
  };
