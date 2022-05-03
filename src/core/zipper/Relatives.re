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

let cat = (_, _) => failwith("todo cat + better name");

let prepend = (d: Direction.t, tiles, rs: t) => {
  let siblings = Siblings.prepend(d, tiles, rs.siblings);
  {...rs, siblings};
};

let nibs = _ => failwith("todo nibs");

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

let remold = (rel: t): list(t) => {
  open ListUtil.Syntax;
  let+ ancestors = Ancestors.remold(rel.ancestors)
  and+ siblings = Siblings.remold(rel.siblings);
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

let regrout = _ => failwith("todo regrout");

// let remove = (selection: Selection.t, relatives: t): t => {
//   switch (Tiles.nibs(selection.content)) {
//   | None => relatives
//   | Some(_nibs) =>
//     let (_gs, rs) = failwith("todo gs rs");
//     // relatives |> cons_piece(Right, Grout(Grout.mk(nibs))) |> split_grouts;
//     let gs = failwith("todo gs");
//     // Grouts.Frame.regrout(gs, Ancestors.sort(rs.ancestors));
//     let siblings = Siblings.(concat([of_grouts(gs), rs.siblings]));
//     reassemble({...rs, siblings});
//   };
// };

// let insert = ({focus, content}: Selection.t, relatives: t): t =>
//   switch (Tiles.nibs(content)) {
//   | None => relatives
//   | Some(inner) =>
//     let outer = nibs(relatives);
//     let _nibs_l = Nibs.fitting((fst(outer), fst(inner)));
//     let _nibs_r = Nibs.fitting((snd(inner), snd(outer)));
//     let (_gs, rs) = failwith("todo gs, rs");
//     // relatives
//     // |> cons_piece(Left, Grout(Grout.mk(nibs_l)))
//     // |> cons_piece(Right, Grout(Grout.mk(nibs_r)))
//     // |> split_grouts;
//     let gs = failwith("todo gs");
//     // Grouts.Frame.regrout(gs, Ancestors.sort(rs.ancestors));
//     let siblings =
//       Siblings.(
//         concat([of_grouts(gs), rs.siblings]) |> prepend(focus, content)
//       );
//     reassemble({...rs, siblings});
//   };
