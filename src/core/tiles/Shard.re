open Sexplib.Std;

module Index = {
  [@deriving sexp]
  type t = int;
};

[@deriving sexp]
type t = {
  tile: (Id.t, Label.t),
  index: Index.t,
  nibs: Nibs.t,
};

let label = (shard: t) => List.nth(snd(shard.tile), shard.index);

let s_of_tile = (_, _, _) => failwith("todo s_of_tile");

module Ctx = {
  type shard = t;
  type t = Id.Map.t(list(shard));

  let lookup = (_, _) => failwith("todo Shard.Ctx.lookup");

  let add = (shard: shard, ctx: t) => {
    let (id, _) = shard.tile;
    let shards = [shard, ...lookup(id, ctx)];
    Id.Map.add(id, shards, ctx);
  };
};
