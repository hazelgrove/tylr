open Sexplib.Std;
open Util;

[@deriving sexp]
type t = list(Grout.t);

// let mk = ((l, r) as nibs: Nibs.t, s: Sort.t): t =>
//   if (Nib.fits(l, r)) {
//     [];
//   } else if (l.sort == r.sort) {
//     [Grout.mk(nibs)];
//   } else {
//     let adjust = (nib: Nib.t) =>
//       nib.sort == s || nib.shape != Convex
//         ? (nib, [])
//         : (
//           {...nib, shape: Concave(Precedence.min)},
//           [Nibs.of_hole(nib.sort)],
//         );
//     let (l, pre) = adjust(l);
//     let (r, suf) = adjust(r);
//     List.map(Grout.mk, pre @ [(l, r)] @ suf);
//   };

let nibs = (gs: t) =>
  switch (gs, ListUtil.split_last_opt(gs)) {
  | ([], _)
  | (_, None) => None
  | ([hd, ..._], Some((_, last))) => Some((fst(hd.nibs), snd(last.nibs)))
  };

// let regrout = (gs, s) =>
//   switch (nibs(gs)) {
//   | None => gs
//   | Some(nibs) => mk(nibs, s)
//   };

module Frame = {
  type t = ListFrame.t(Grout.t);

  let to_list = ListFrame.to_list;
  // let regrout = (gs_frame: t, s: Sort.t): t => {
  //   let gs = to_list(gs_frame);
  //   let gs' = regrout(gs, s);
  //   gs' == gs ? gs_frame : ([], gs');
  // };
};

// let regrout = (gs: t): t =>
//   List.fold_left(
//     ((pushed, plain, popped), g) => {
//       let s = g.sort;
//       let l = fst(g.nibs);
//       let r = snd(g.nibs);
//       if (s == l.sort && s == r.sort) {
//         let plain =
//           switch (plain) {
//           | None => Some(g)
//           | Some(g') => Grout.mk_opt((fst(g'.nibs), r), s)
//           };
//         (pushed, plain, popped);
//       } else if (s == l.sort) {
//         // always push?
//         // asymmetry that makes sense given empty stack but still need to review
//         ([g, ...pushed], None, )
//       }
//     }
