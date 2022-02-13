include Tile.Shard;

module Index = {
  [@deriving sexp]
  type t = index;
  let compare = Int.compare;
};

module Labeled = {
  [@deriving sexp]
  type t = labeled;

  let s_of_tile =
      (id: Tile.Id.t, mold: Tile.Mold.t, label: Tile.Label.t): list(t) =>
    label
    |> List.mapi((index, _) =>
         {index, tile: (id, label), nibs: Tile.nibs(~index, mold)}
       );
};

module Placeholder = {
  [@deriving sexp]
  type t = placeholder;
};

/**
 * to be raised if matching shards are found
 * to have been assigned inconsistent nibs
 */
exception Inconsistent_nibs;

// module Index = {
//   [@deriving sexp]
//   type t = int;
//   let compare = Int.compare;
// };

// module Label = {
//   [@deriving sexp]
//   type t = Token.t;
// };

// type t = {
//   delim,
//   nibs: Nibs.t,
// }
// and delim =
//   | Hole(list(Tile.Id.t), Precedence.t)
//   | Token(Tile.Id.t, Tile.Label.t, Index.t);

// [@deriving sexp]
// type t = {
//   tile: (Tile.Id.t, Tile.Label.t),
//   index: Index.t,
//   nibs: Nibs.t,
// };

// let id = shard => fst(shard.tile);

// let label = (shard: t): Label.t => {
//   let (_, tile_label) = shard.tile;
//   List.nth(tile_label, shard.index);
// };

// let of_tile = (index: Index.t, tile: Tile.t): t => {
//   tile: (tile.id, Tile.label(tile)),
//   index,
//   nibs: Tile.nibs(~index, tile.mold),
// };
// let of_tile_frame = (index: Index.t, tile: Tile.Frame.t): t => {
//   tile: (tile.id, Tile.Frame.label(tile)),
//   index,
//   nibs: Tile.nibs(~index, tile.mold),
// };

module Map = {
  include Map.Make(Index);
};

module Ctx = {
  type shard = t;
  type t = Tile.Map.t(list(Labeled.t));

  let lookup = (_, _) => failwith("todo Shard.Ctx.lookup");

  let add = (shard: Labeled.t, ctx: t) => {
    let (id, _) = shard.tile;
    let shards = [shard, ...lookup(id, ctx)];
    Tile.Map.add(id, shards, ctx);
  };
};

// assumes input shards are of same tile
let is_assignable = (shards: list(Labeled.t), mold: Tile.Mold.t) =>
  shards
  |> List.for_all(shard => Tile.nibs(~index=shard.index, mold) == shard.nibs);

// assumes input shards are of same tile
let assignable_molds = (shards: list(Labeled.t)): list(Tile.Mold.t) =>
  switch (shards) {
  | [] => []
  | [{tile: (_, label), _}, ..._] =>
    List.filter(is_assignable(shards), Tile.assignable_molds(label))
  };

let assignable_nibs =
    // l is intended to be left neighbor nib that is
    // used to filter down to sort-consistent nibs
    (~l as _: option(Nib.t)=?, ctx: Ctx.t, shard: Labeled.t) => {
  let (id, label) = shard.tile;
  Tile.assignable_molds(label)
  |> List.filter(is_assignable(Ctx.lookup(id, ctx)))
  |> List.map(Tile.nibs(~index=shard.index));
};
