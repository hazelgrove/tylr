open Util;
open Core;

type segment = list((Measurement.t, piece))
and piece =
  | Grout(Grout.t)
  | Shard(Shard.t)
  | Tile(tile)
and tile = {
  id: Id.t,
  label: Tile.Label.t,
  mold: Mold.t,
  children: list(segment),
};

type siblings = (segment, segment);

type ancestor = {
  id: Id.t,
  label: Tile.Label.t,
  mold: Mold.t,
  children: ListFrame.t(segment),
};

type generation = ((Measurement.t, ancestor), siblings);
type ancestors = list(generation);

type relatives = {
  siblings,
  ancestors,
};

type selection = {
  focus: Direction.t,
  content: segment,
};

type zipper = {
  // id_gen: IdGen.t,
  selection,
  // backpack: Backpack.t,
  relatives,
};

let of_zipper = _: zipper => failwith("todo Measured.of_zipper");
