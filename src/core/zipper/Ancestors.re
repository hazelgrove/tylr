open Util;

[@deriving show]
type generation = (Ancestor.t, Siblings.t);

[@deriving show]
type t = list(generation);

let empty = [];

let sort =
  fun
  | [] => Sort.root
  | [(a, _), ..._] => Ancestor.sort(a);

let remold = (ancestors: t): list(t) =>
  List.fold_right(
    ((a, sibs), remolded) => {
      open ListUtil.Syntax;
      let+ ancestors = remolded
      and+ sibs = Siblings.remold(sibs)
      and+ a = Ancestor.remold(a);
      [(a, sibs), ...ancestors];
    },
    ancestors,
    [empty],
  );

let sort_rank = (ancestors: t) =>
  List.fold_right(
    ((a, sibs), (s, rank)) => {
      let rank =
        rank
        + Siblings.sort_rank(sibs, s)
        + Ancestor.sort_rank(a, Siblings.sorts(sibs, s));
      let s' = Ancestor.sort(a);
      (s', rank);
    },
    ancestors,
    (Sort.root, 0),
  )
  |> snd;

let shape_rank = (ancestors: t): int =>
  List.fold_right(
    ((_, sibs), rank) => Siblings.shape_rank(sibs) + rank,
    ancestors,
    0,
  );

let regrout =
  List.map(((a: Ancestor.t, sibs: Siblings.t)) => {
    let (n_l, n_r) = Mold.nibs(a.mold);
    let (s_l, s_r) = (n_l.shape, n_r.shape);
    let (pre, suf) = Siblings.regrout(sibs);
    let pre =
      switch (pre) {
      | [Grout(g), ...pre'] => Grout.fits_shape(g, s_l) ? pre : pre'
      | _ =>
        Nib.Shape.fits(s_l, Siblings.Prefix.shape(pre))
          ? pre : [Grout(Grout.mk_fits_shape(s_l)), ...pre]
      };
    let suf =
      switch (suf) {
      | [Grout(g), ...suf'] => Grout.fits_shape(g, s_r) ? suf : suf'
      | _ =>
        Nib.Shape.fits(s_r, Siblings.Prefix.shape(suf))
          ? suf : [Grout(Grout.mk_fits_shape(s_r)), ...suf]
      };
    (a, (pre, suf));
  });
