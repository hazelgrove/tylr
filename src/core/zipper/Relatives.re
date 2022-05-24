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
        Siblings.(
          pop(~balanced, d, concat([rs.siblings, siblings', siblings]))
        );
      (p, {siblings, ancestors});
    | _ => None
    }
  };

let disassemble = ({siblings, ancestors}: t): Siblings.t =>
  Siblings.concat([siblings, Ancestors.disassemble(ancestors)]);

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

// called when selection is empty
let regrout = ({siblings, ancestors}: t): t => {
  let ancestors = Ancestors.regrout(ancestors);
  let siblings = {
    let (pre, suf) = Siblings.regrout(siblings);
    let (s_pre, s_suf) = Siblings.shapes(siblings);
    switch (pre, suf) {
    | ([Grout((l, _)), ...pre'], [Grout((_, r)), ...suf']) =>
      // Grout.fits(g, g') ? (pre', suf') : (pre', suf)
      (pre', [Piece.Grout((l, r)), ...suf'])
    | ([Grout((l, r)), ...pre'], _) =>
      if (/* necessary */ Nib.Shape.fits(r, s_suf)) {
        (pre, suf);
      } else if (/* prefix/postfix */ Nib.Shape.fits(l, r)) {
        (
          // change
          [Grout((l, Nib.Shape.flip(r))), ...pre'],
          suf,
        );
      } else {
        (pre', suf);
      }
    | (_, [Grout((l, r)), ...suf']) =>
      if (/* necessary */ Nib.Shape.fits(s_pre, l)) {
        (pre, suf);
      } else if (/* prefix/postfix */ Nib.Shape.fits(l, r)) {
        (
          // change
          pre,
          [Grout((Nib.Shape.flip(l), r)), ...suf'],
        );
      } else {
        (pre, suf');
      }
    | _ =>
      Nib.Shape.fits(s_pre, s_suf)
        ? (pre, suf) : (pre, [Grout(Grout.mk_fits_shape(s_suf)), ...suf])
    };
  };
  {siblings, ancestors};
};

let prepend_generation = ((a, sibs): Ancestors.generation, rs: t): t => {
  siblings: Siblings.empty,
  ancestors: [(a, Siblings.concat([sibs, rs.siblings])), ...rs.ancestors],
};
let prepend_siblings = (sibs: Siblings.t, rs: t): t => {
  ...rs,
  siblings: Siblings.concat([sibs, rs.siblings]),
};

let concat = (rss: list(t)): t =>
  List.fold_right(
    (rs: t, cat: t) =>
      List.fold_right(prepend_generation, rs.ancestors, cat)
      |> prepend_siblings(rs.siblings),
    rss,
    empty,
  );

let rec reassemble = (rs: t): t => {
  let siblings = Siblings.reassemble(rs.siblings);
  switch (Siblings.shards(siblings)) {
  | ([], _)
  | (_, []) => {...rs, siblings}
  | ([_, ..._], [s, ..._]) =>
    switch (
      siblings
      |> Siblings.split_by_matching_shard(s.tile_id)
      |> TupleUtil.map2(Aba.trim)
    ) {
    | (None, None) => {...rs, siblings}
    | (None, Some((inner_r, match_r, outer_r))) =>
      let {siblings: (l, r), ancestors} =
        reassemble({...rs, siblings: (fst(rs.siblings), outer_r)});
      {
        siblings: (
          l,
          Segment.concat([inner_r, Ancestor.Match.Suffix.join(match_r), r]),
        ),
        ancestors,
      };
    | (Some((inner_l, match_l, outer_l)), None) =>
      let {siblings: (l, r), ancestors} =
        reassemble({...rs, siblings: (outer_l, snd(rs.siblings))});
      {
        siblings: (
          Segment.concat([inner_l, Ancestor.Match.Suffix.join(match_l), l]),
          r,
        ),
        ancestors,
      };
    | (Some((inner_l, match_l, outer_l)), Some((inner_r, match_r, outer_r))) =>
      let match = (match_l, match_r);
      let rs_inner =
        switch (Ancestor.Match.complete(match)) {
        | None => {
            siblings:
              Siblings.concat([
                (inner_l, inner_r),
                Ancestor.Match.join(match),
              ]),
            ancestors: Ancestors.empty,
          }
        | Some(a) => {
            siblings: (inner_l, inner_r),
            ancestors: [(a, Siblings.empty)],
          }
        };
      let rs_outer = reassemble({...rs, siblings: (outer_l, outer_r)});
      concat([rs_inner, rs_outer]);
    }
  };
};
