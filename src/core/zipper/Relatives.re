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

let prepend = (~connect=false, d: Direction.t, tiles, relatives: t) => {
  let s = Ancestors.sort(relatives.ancestors);
  let (tiles, siblings) =
    connect
      ? Tiles.connect(~insert=tiles, relatives.siblings, s)
      : (tiles, relatives.siblings);
  let siblings = Siblings.prepend(d, tiles, siblings);
  {...relatives, siblings};
};
let connect = prepend(~connect=true, Left, Tiles.empty);

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
