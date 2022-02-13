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
