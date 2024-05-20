open Util;

module Base = {
  type t('a) = ('a, Mold.t);
  let map = (f, (a, mold): t(_)) => (f(a), mold);
};
include Base;

module T = {
  type t = (Label.t, Mold.t);
};
module NT = {
  type t = (Sort.t, Bound.t(Mold.t));
  let root = (Sort.root, Bound.Root);
  let sort = fst;
  let bounds =
    fun
    | (sort, Bound.Node(mold: Mold.t)) when sort == mold.sort =>
      Mold.bounds(mold)
    | _ => Bound.(Root, Root);
};
module Sym = {
  type t = Base.t(Grammar.Sym.t);
  let get_nt =
    fun
    | Bound.Root => Some(Bound.Root)
    | Node((Sym.NT(nt), mold)) => Some(Node((nt, mold)))
    | Node((T(_), _)) => None;
};

// open Util;
// // molded mtrlized syms
// module T = {
//   [@deriving (sexp, yojson, ord)]
//   type t = (Mtrl.T.t, Mold.t);
//   let pp = (out, (mtrl, mold): t) => {
//     let (l, r) = Mold.display(mold);
//     Fmt.pf(out, "%s%a%s", l, Mtrl.T.pp, mtrl, r);
//   };
//   let show = Fmt.to_to_string(pp);
//   let mtrl = ((mtrl, _): t) => mtrl;
//   let padding = ((mtrl, _): t) => Mtrl.Labeled.padding(mtrl);
// };
// module NT = {
//   [@deriving (sexp, yojson, ord)]
//   type t = (Mtrl.NT.t, Mold.t);
//   let pp = (out, (mtrl, _): t) => Fmt.pf(out, "%a", Mtrl.NT.pp, mtrl);
//   let show = Fmt.to_to_string(pp);
//   let mtrl =
//     fun
//     | Bound.Root => Mtrl.Sorted.root
//     | Node((mtrl, _)) => mtrl;
//   let bounds =
//     fun
//     | Bound.Root => Bound.(Root, Root)
//     | Node((_, mold)) => Mold.bounds(mold);
// };
// module Sym = {
//   include Sym;
//   [@deriving (show({with_path: false}), sexp, yojson, ord)]
//   type t = Sym.t(T.t, NT.t);
// };
