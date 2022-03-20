open Util;

[@deriving show]
type t = {
  siblings: Siblings.t,
  ancestors: Ancestors.t,
};

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
      let+ (p, siblings) = Siblings.(pop(d, concat([siblings, siblings'])));
      (p, {siblings, ancestors});
    | _ => None
    }
  };

// let split_tile = (d, relatives): option((Tile.t, t)) =>
//   Siblings.pop(d, relatives.siblings);

// let pop_balanced = (from: Direction.t, rs: t): option((Tile.t, t)) =>
//   switch (Siblings.pop(from, rs.siblings)) {
//   | Some(_) =>
//     open OptUtil.Syntax;
//     let+ (tile, siblings) = Siblings.pop_balanced(from, rs.siblings);
//     (tile, {...rs, siblings});
//   | None =>
//     switch (rs.ancestors) {
//     | [] => None
//     | [(ancestor, siblings), ...ancestors] =>
//       open OptUtil.Syntax;
//       let siblings' = Ancestor.disassemble(ancestor);
//       let+ (tile, siblings) =
//         Siblings.(pop_balanced(from, concat([siblings', siblings])));
//       (tile, {siblings, ancestors});
//     }
//   };

let reassemble = _ => failwith("todo reassemble");

let default_mold = (_, _) => failwith("todo default_mold");

let rec disassemble_grouts = (rs: t): t =>
  switch (rs.ancestors) {
  | []
  | [(Intact(_), _), ..._]
  | [(Pieces(((Shard(_), _), _) | (_, (Shard(_), _))), _), ..._] => rs
  | [
      (Pieces(((Grout(_), _), (Grout(_), _))) as ancestor, siblings),
      ...ancestors,
    ] =>
    let siblings =
      Siblings.concat([
        Ancestor.disassemble(ancestor),
        siblings,
        rs.siblings,
      ]);
    disassemble_grouts({siblings, ancestors});
  };

// note: may disassemble ancestors, does not reassemble
let split_grouts = (relatives: t): (Grouts.Frame.t, t) => {
  let relatives = disassemble_grouts(relatives);
  let (gs, siblings) = Siblings.split_grouts(relatives.siblings);
  (gs, {...relatives, siblings});
};

let remove = (selection: Selection.t, relatives: t): t => {
  switch (Tiles.nibs(selection.content)) {
  | None => relatives
  | Some(_nibs) =>
    let (_gs, rs) = failwith("todo gs rs");
    // relatives |> cons_piece(Right, Grout(Grout.mk(nibs))) |> split_grouts;
    let gs = failwith("todo gs");
    // Grouts.Frame.regrout(gs, Ancestors.sort(rs.ancestors));
    let siblings = Siblings.(concat([of_grouts(gs), rs.siblings]));
    reassemble({...rs, siblings});
  };
};

let insert = ({focus, content}: Selection.t, relatives: t): t =>
  switch (Tiles.nibs(content)) {
  | None => relatives
  | Some(inner) =>
    let outer = nibs(relatives);
    let _nibs_l = Nibs.fitting((fst(outer), fst(inner)));
    let _nibs_r = Nibs.fitting((snd(inner), snd(outer)));
    let (_gs, rs) = failwith("todo gs, rs");
    // relatives
    // |> cons_piece(Left, Grout(Grout.mk(nibs_l)))
    // |> cons_piece(Right, Grout(Grout.mk(nibs_r)))
    // |> split_grouts;
    let gs = failwith("todo gs");
    // Grouts.Frame.regrout(gs, Ancestors.sort(rs.ancestors));
    let siblings =
      Siblings.(
        concat([of_grouts(gs), rs.siblings]) |> prepend(focus, content)
      );
    reassemble({...rs, siblings});
  };

// let sort_stack = ({siblings, ancestors}: t): (Sort.t, Sort.Stack.t) => {
//   let (prefix, _) = siblings;
//   switch (Siblings.Affix.rev(prefix)) {
//   |
//   }
// }

// let insert = ({focus, content}: Selection.t, relatives: t): t =>
//   switch (Tiles.nibs(content)) {
//   | None => relatives
//   | Some(inner) =>

//   }

// let insert = ({focus: _, content}: Selection.t, relatives: t): t =>
//   switch (Tiles.nibs(content)) {
//   | None => relatives
//   | Some(inner_nibs) =>
//     let inner_ss = Tiles.min_sort_stacks(content);
//     let (outer_nibs, outer_ss) = Relatives.sort_stacks(relatives);
