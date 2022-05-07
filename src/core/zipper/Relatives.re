open Util;

[@deriving show]
type t = {
  siblings: Siblings.t,
  ancestors: Ancestors.t,
};

module Prefix = Siblings.Prefix;
module Suffix = Siblings.Suffix;

let empty = {siblings: Siblings.empty, ancestors: Ancestors.empty};

let push = (d: Direction.t, p: Piece.t, rs: t): t => {
  ...rs,
  siblings: Siblings.push(d, p, rs.siblings),
};

let prepend = (d: Direction.t, tiles: Segment.t, rs: t): t => {
  let siblings = Siblings.prepend(d, tiles, rs.siblings);
  {...rs, siblings};
};

let pop = (~balanced: bool, d: Direction.t, rs: t): option((Piece.t, t)) =>
  switch (Siblings.pop(~balanced, d, rs.siblings)) {
  | Some((p, siblings)) => Some((p, {...rs, siblings}))
  | None =>
    switch (rs.ancestors) {
    | [(ancestor, siblings), ...ancestors] when !balanced =>
      open OptUtil.Syntax;
      let siblings' = Ancestor.disassemble(ancestor);
      let+ (p, siblings) =
        Siblings.(pop(~balanced, d, concat([siblings, siblings'])));
      (p, {siblings, ancestors});
    | _ => None
    }
  };

let reassemble = (rs: t) => {
  let rev = Aba.rev(Fun.id, Fun.id);

  let rec go = ((rev_pre, rev_suf), ancestors) => {
    let flatten = () => {
      let pre = Prefix.Stack.flatten(rev(rev_pre));
      let suf = Suffix.Stack.flatten(rev(rev_suf));
      {siblings: (pre, suf), ancestors};
    };
    let (hd_pre, tl_pre) = rev_pre;
    let (hd_suf, tl_suf) = rev_suf;
    switch (tl_pre, tl_suf) {
    | ([(m_pre, seg_pre), ...tl_pre], [(m_suf, seg_suf), ...tl_suf]) =>
      switch (Ancestor.Match.complete((m_pre, m_suf))) {
      | Some(ancestor) =>
        let rev_pre = (seg_pre, tl_pre);
        let rev_suf = (seg_suf, tl_suf);
        let ancestors = [(ancestor, (hd_pre, hd_suf)), ...ancestors];
        go((rev_pre, rev_suf), ancestors);
      | None => flatten()
      }
    | _ => flatten()
    };
  };

  let (pre, suf) = Siblings.reassemble(rs.siblings);
  let matched_pre = Prefix.match(pre);
  let matched_suf = Suffix.match(suf);
  go((rev(matched_pre), rev(matched_suf)), rs.ancestors);
};

let default_mold = (_, _) => failwith("todo default_mold");

let remold = ({siblings, ancestors}: t): list(t) => {
  open ListUtil.Syntax;
  let+ ancestors = Ancestors.remold(ancestors)
  and+ siblings = Siblings.remold(siblings);
  {ancestors, siblings};
};

let sort_rank = ({siblings, ancestors}: t) => {
  let s = Ancestors.sort(ancestors);
  let (s_l, s_r) = Siblings.sorts(siblings, s);
  Ancestors.sort_rank(ancestors)
  + Siblings.sort_rank(siblings, s)
  + Bool.to_int(!Sort.consistent(s_l, s_r));
};

let shape_rank = ({siblings, ancestors}: t) => {
  let (l, r) = Siblings.shapes(siblings);
  Ancestors.shape_rank(ancestors)
  + Siblings.shape_rank(siblings)
  + Bool.to_int(!Nib.Shape.fits(l, r));
};

let regrout = ({siblings, ancestors}: t): t => {
  let ancestors = Ancestors.regrout(ancestors);
  let siblings = {
    let (pre, suf) = Siblings.regrout(siblings);
    let (s_pre, s_suf) = Siblings.shapes(siblings);
    // relies on invariant of non-consecutive grout
    switch (pre, suf) {
    | ([Grout(g), ...pre'], _) =>
      Grout.fits(g, s_suf) ? (pre, suf) : (pre', suf)
    | (_, [Grout(g), ...suf']) =>
      Grout.fits(g, s_pre) ? (pre, suf) : (pre, suf')
    | _ =>
      Nib.Shape.fits(s_pre, s_suf)
        ? (pre, suf) : (pre, [Grout(Grout.mk_fits(s_suf)), ...suf])
    };
  };
  {siblings, ancestors};
};
