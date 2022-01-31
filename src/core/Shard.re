open Sexplib.Std;

module Index = {
  [@deriving sexp]
  type t = int;
  let compare = Int.compare;
};

module Label = {
  [@deriving sexp]
  type t = Token.t;
};

[@deriving sexp]
type t = {
  tile: (Tile.Id.t, Tile.Label.t),
  index: Index.t,
  nibs: Nibs.t,
};

let id = shard => fst(shard.tile);

let label = (shard: t): Label.t => {
  let (_, tile_label) = shard.tile;
  List.nth(tile_label, shard.index);
};

let of_tile = (index: Index.t, tile: Tile.t): t => {
  tile: (tile.id, Tile.label(tile)),
  index,
  nibs: Tile.nibs(~index, tile.mold),
};
let of_tile_frame = (index: Index.t, tile: Tile.Frame.t): t => {
  tile: (tile.id, Tile.Frame.label(tile)),
  index,
  nibs: Tile.nibs(~index, tile.mold),
};

module Map = {
  include Map.Make(Index);
};

module Ctx = {
  type shard = t;
  type t = Tile.Map.t(list(shard));

  let lookup = (_, _) => failwith("todo Shard.Ctx.lookup");

  let add = (shard: shard, ctx: t) => {
    let (id, _) = shard.tile;
    let shards = [shard, ...lookup(id, ctx)];
    Tile.Map.add(id, shards, ctx);
  };
};

let consistent = (shards: list(t), mold: Tile.Mold.t) =>
  shards
  |> List.for_all(shard => Tile.nibs(~index=shard.index, mold) == shard.nibs);

let assignable_nibs =
    // l is intended to be left neighbor nib that is
    // used to filter down to sort-consistent nibs
    (~l as _: option(Nib.t)=?, ctx: Ctx.t, shard: t) => {
  let (id, label) = shard.tile;
  Tile.assignable_molds(label)
  |> List.filter(consistent(Ctx.lookup(id, ctx)))
  |> List.map(Tile.nibs(~index=shard.index));
};
