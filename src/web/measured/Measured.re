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

let shards: segment => list(Shard.t) =
  List.filter_map(
    fun
    | (_, Shard(s)) => Some(s)
    | _ => None,
  );

// let split_segment_by_matching_shard = (tile_id: Id.t): (segment => Aba.t(segment, Shard.t)) =>
//   Aba.split(
//     fun
//     | Shard(s) when s.tile_id == tile_id => Either.R(s)
//     | p => L(p)
//   );

let of_segment = _: segment => failwith("todo Measured.of_segment");
let of_zipper = _: zipper => failwith("todo Measured.of_zipper");
