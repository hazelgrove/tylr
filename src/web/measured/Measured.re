open Util;
open Core;

type segment = list((Layout.measurement, piece))
and piece =
  | Whitespace(Whitespace.t)
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

module Anc = {
  type t = {
    id: Id.t,
    label: Tile.Label.t,
    mold: Mold.t,
    children: ListFrame.t(segment),
  };
};

type generation = ((Layout.measurement, Anc.t), siblings);
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

// TODO fix weird default
let rec of_segment = (~origin=1, seg: Segment.t): (int, segment) =>
  seg
  |> ListUtil.fold_left_map(
       (origin, p) => {
         let (m, p) = of_piece(~origin, p);
         (m.origin + m.length + 1, (m, p));
       },
       origin,
     )
and of_piece = (~origin=0, p: Piece.t): (Layout.measurement, piece) =>
  switch (p) {
  // TODO(d) change once w includes newlines
  | Whitespace(w) => ({origin, length: 1}, Whitespace(w))
  | Grout(g) => ({origin, length: 1}, Grout(g))
  | Shard(s) => (
      {origin, length: Unicode.length(Shard.Label.token(s.label))},
      Shard(s),
    )
  | Tile(t) =>
    let (hd, tl) = ListUtil.split_first(t.label);
    let (origin', children) =
      List.combine(t.children, tl)
      |> ListUtil.fold_left_map(
           (origin, (child, token)) => {
             let (origin, child) = of_segment(~origin, child);
             (origin + Unicode.length(token) + 1, child);
           },
           origin + Unicode.length(hd) + 1,
         );
    // TODO -1
    (
      {origin, length: origin' - origin - 1},
      Tile({id: t.id, label: t.label, mold: t.mold, children}),
    );
  };

let of_zipper = _: zipper => failwith("todo Measured.of_zipper");
