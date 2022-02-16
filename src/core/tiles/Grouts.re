open Sexplib.Std;
open Util;

[@deriving sexp]
type t = list(Grout.t);

let nibs = (gs: t) =>
  switch (gs, Util.ListUtil.split_last_opt(gs)) {
  | ([], _)
  | (_, None) => None
  | ([hd, ..._], Some((_, last))) => Some((fst(hd.nibs), snd(last.nibs)))
  };

let regrout = (gs: t, s: Sort.t): t =>
  switch (nibs(gs)) {
  | None => gs
  | Some((l, r) as nibs) =>
    if (Nib.fits(l, r)) {
      [];
    } else if (l.sort == r.sort) {
      [{nibs: nibs}];
    } else {
      let adjust = (nib: Nib.t) =>
        nib.sort == s || nib.shape != Convex
          ? (nib, [])
          : (
            {...nib, shape: Concave(Precedence.min)},
            [Nibs.of_hole(nib.sort)],
          );
      let (l, pre) = adjust(l);
      let (r, suf) = adjust(r);
      pre @ [(l, r)] @ suf |> List.map(nibs => Grout.{nibs: nibs});
    }
  };

module Frame = {
  type t = ListFrame.t(Grout.t);

  let to_list = ListFrame.to_list;

  let regrout = (gs_frame: t, s: Sort.t): t => {
    let gs = to_list(gs_frame);
    let gs' = regrout(gs, s);
    gs' == gs ? gs_frame : ([], gs');
  };
};
