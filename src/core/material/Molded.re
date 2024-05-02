open Util;

// molded mtrlized syms
module T = {
  [@deriving (sexp, yojson, ord)]
  type t = (Mtrl.T.t, Mold.t);
  let pp = (out, (mtrl, mold): t) => {
    let (l, r) = Mold.display(mold);
    Fmt.pf(out, "%s%a%s", l, Mtrl.T.pp, mtrl, r);
  };
  let show = Fmt.to_to_string(pp);
  let mtrl = ((mtrl, _): t) => mtrl;
  let padding = ((mtrl, _): t) => Mtrl.Labeled.padding(mtrl);
};
module NT = {
  [@deriving (sexp, yojson, ord)]
  type t = (Mtrl.NT.t, Mold.t);
  let pp = (out, (mtrl, _): t) => Fmt.pf(out, "%a", Mtrl.NT.pp, mtrl);
  let show = Fmt.to_to_string(pp);
  let mtrl =
    fun
    | Bound.Root => Mtrl.Sorted.root
    | Node((mtrl, _)) => mtrl;
  let bounds =
    fun
    | Bound.Root => Bound.(Root, Root)
    | Node((_, mold)) => Mold.bounds(mold);
};
module Sym = {
  include Sym;
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Sym.t(T.t, NT.t);
};
