[@deriving sexp]
type t = {
  siblings: Siblings.t,
  ancestors: Ancestors.t,
};

let cons = (d: Direction.t, tile: Tile.t, relatives: t): t => {
  ...relatives,
  siblings: Siblings.cons(d, tile, relatives.siblings),
};
let cons_shard = (d, shard, relatives) =>
  cons(d, Tile.of_shard(shard), relatives);

let prepend = (~connect=false, d, tiles, relatives: t) => {
  let s = Ancestors.sort(relatives.ancestors);
  let (tiles, siblings) =
    connect
      ? Tiles.connect(~insert=tiles, relatives.siblings, s)
      : (tiles, relatives.siblings);
  let siblings = Siblings.prepend(d, tiles, siblings);
  {...relatives, siblings};
};
let connect = prepend(~connect=true, Left, Tiles.empty);

let split_shard =
    (d: Direction.t, {siblings, ancestors}: t): option((Shard.t, t)) =>
  switch (Siblings.split_shard(d, siblings)) {
  | Some((shard, siblings)) => Some((shard, {siblings, ancestors}))
  | None =>
    switch (ancestors) {
    | [] => None
    | [ancestor, ...ancestors] =>
      open OptUtil.Syntax;
      let siblings' = Ancestor.disassemble(ancestor);
      let+ (shard, siblings) =
        Siblings.(split_shard(d, concat([siblings, siblings'])));
      (shard, {siblings, ancestors});
    }
  };

let split_tile = (d, relatives) =>
  Siblings.split_tile(d, relatives.siblings);

let split_hd =
    (d: Direction.t, {siblings, ancestors}: t): option((Tile.t, t)) => {
  switch (Siblings.split_hd(d, siblings)) {
  | Some((tile, siblings)) => Some((tile, siblings, ancestors))
  | None =>
    switch (ancestors) {
    | [] => None
    | [ancestor, ...ancestors] =>
      open OptUtil.Syntax;
      let siblings' = Parser.disassemble_ancestor(ancestor);
      let+ (piece, siblings) =
        Siblings.(split_hd(d, concat([siblings, siblings'])));
      (piece, siblings, ancestors);
    }
  };
};
