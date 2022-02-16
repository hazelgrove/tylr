open Util;

[@deriving sexp]
type t = {
  siblings: Siblings.t,
  ancestors: Ancestors.t,
};

let empty = {siblings: Siblings.empty, ancestors: Ancestors.empty};

let cons = (d: Direction.t, tile: Tile.t, relatives: t): t => {
  ...relatives,
  siblings: Siblings.cons(d, tile, relatives.siblings),
};
let cons_piece = (d, piece, relatives) =>
  cons(d, Tile.of_piece(piece), relatives);

let cat = (_, _) => failwith("todo cat + better name");

let prepend = (d: Direction.t, tiles, rs: t) => {
  let siblings = Siblings.prepend(d, tiles, rs.siblings);
  {...rs, siblings};
};

let nibs = _ => failwith("todo nibs");

let split_piece =
    (d: Direction.t, {siblings, ancestors}: t): option((Piece.t, t)) =>
  switch (Siblings.split_piece(d, siblings)) {
  | Some((piece, siblings)) => Some((piece, {siblings, ancestors}))
  | None =>
    switch (ancestors) {
    | [] => None
    | [(ancestor, siblings''), ...ancestors] =>
      open OptUtil.Syntax;
      let siblings' = Ancestor.disassemble(ancestor);
      let+ (piece, siblings) =
        Siblings.(split_piece(d, concat([siblings, siblings', siblings''])));
      (piece, {siblings, ancestors});
    }
  };

let split_tile = (d, relatives): option((Tile.t, t)) =>
  Siblings.split_tile(d, relatives.siblings);

let split_hd =
    (d: Direction.t, {siblings, ancestors}: t): option((Tile.t, t)) => {
  switch (Siblings.split_hd(d, siblings)) {
  | Some((tile, siblings)) => Some((tile, {siblings, ancestors}))
  | None =>
    switch (ancestors) {
    | [] => None
    | [(ancestor, siblings''), ...ancestors] =>
      open OptUtil.Syntax;
      let siblings' = Ancestor.disassemble(ancestor);
      let+ (piece, siblings) =
        Siblings.(split_hd(d, concat([siblings, siblings', siblings''])));
      (piece, {siblings, ancestors});
    }
  };
};

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
  | Some(nibs) =>
    let (gs, rs) =
      relatives |> cons_piece(Right, Grout({nibs: nibs})) |> split_grouts;
    let gs = Grouts.Frame.regrout(gs, Ancestors.sort(rs.ancestors));
    let siblings = Siblings.(concat([of_grouts(gs), rs.siblings]));
    reassemble({...rs, siblings});
  };
};

let insert = ({focus, content}: Selection.t, relatives: t): t =>
  switch (Tiles.nibs(content)) {
  | None => relatives
  | Some(inner) =>
    let outer = nibs(relatives);
    let nibs_l = Nibs.fitting((fst(outer), fst(inner)));
    let nibs_r = Nibs.fitting((snd(inner), snd(outer)));
    let (gs, rs) =
      relatives
      |> cons_piece(Left, Grout({nibs: nibs_l}))
      |> cons_piece(Right, Grout({nibs: nibs_r}))
      |> split_grouts;
    let gs = Grouts.Frame.regrout(gs, Ancestors.sort(rs.ancestors));
    let siblings =
      Siblings.(
        concat([of_grouts(gs), rs.siblings]) |> prepend(focus, content)
      );
    reassemble({...rs, siblings});
  };
