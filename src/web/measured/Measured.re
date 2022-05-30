open Util;
open Core;

// [@deriving show]
// type measurement = {
//   origin: int,
//   length: int,
// };

type t = {
  tiles: Id.Map.t(list((list(int), Layout.measurement))),
  grout: Id.Map.t(Layout.measurement),
  whitespace: Id.Map.t(Layout.measurement),
};

let empty = {
  tiles: Id.Map.empty,
  grout: Id.Map.empty,
  whitespace: Id.Map.empty,
};

let add_t = (t: Tile.t, m, map) => {
  ...map,
  tiles:
    map.tiles
    |> Id.Map.update(
         t.id,
         fun
         | None => Some([(t.shards, m)])
         | Some(ms) => Some([(t.shards, m), ...ms]),
       ),
};
let add_g = (g: Grout.t, m, map) => {
  ...map,
  grout: map.grout |> Id.Map.add(g.id, m),
};
let add_w = (w: Whitespace.t, m, map) => {
  ...map,
  whitespace: map.whitespace |> Id.Map.add(w.id, m),
};

let singleton_w = (w, m) => empty |> add_w(w, m);
let singleton_g = (g, m) => empty |> add_g(g, m);
let singleton_t = (t, m) => empty |> add_t(t, m);

let find_w = (w: Whitespace.t, map) => Id.Map.find(w.id, map.whitespace);
let find_g = (g: Grout.t, map) => Id.Map.find(g.id, map.grout);
let find_t = (t: Tile.t, map) =>
  List.assoc(t.shards, Id.Map.find(t.id, map.tiles));
let find_p = (p: Piece.t, map) =>
  p
  |> Piece.get(
       w => find_w(w, map),
       g => find_g(g, map),
       t => find_t(t, map),
     );

let union2 = (map: t, map': t) => {
  tiles:
    Id.Map.union((_, ms, ms') => Some(ms @ ms'), map.tiles, map'.tiles),
  grout: Id.Map.union((_, m, _) => Some(m), map.grout, map'.grout),
  whitespace:
    Id.Map.union((_, m, _) => Some(m), map.whitespace, map'.whitespace),
};
let union = List.fold_left(union2, empty);

// TODO fix weird default
let rec of_segment = (~origin=0, seg: Segment.t): (int, t) =>
  seg
  |> ListUtil.fold_left_map((origin, p) => of_piece(~origin, p), origin)
  |> PairUtil.map_snd(union)
//  (m.origin + m.length + 1, (m, p));
and of_piece = (~origin=0, p: Piece.t): (int, t) =>
  switch (p) {
  | Whitespace(w) =>
    // TODO(d) change once w includes newlines
    let length = 1;
    (origin + length, singleton_w(w, {origin, length}));
  | Grout(g) =>
    let length = 1;
    (origin + length, singleton_g(g, {origin, length}));
  | Tile(t) =>
    let (hd, tl) = ListUtil.split_first(t.shards);
    let token = List.nth(t.label);
    let (origin', map) =
      List.combine(t.children, tl)
      |> ListUtil.fold_left_map(
           (origin, (child, i)) => {
             let (origin, map) = of_segment(~origin, child);
             (origin + Unicode.length(token(i)), map);
           },
           origin + Unicode.length(token(hd)),
         )
      |> PairUtil.map_snd(union);
    let length = origin' - origin;
    (origin', map |> add_t(t, {origin, length}));
  };

let length = (seg: Segment.t, map: t): int =>
  switch (seg) {
  | [] => 0
  | [p] =>
    let m = find_p(p, map);
    m.length;
  | [hd, ...tl] =>
    let first = find_p(hd, map);
    let last = find_p(ListUtil.last(tl), map);
    last.origin - first.origin + last.length;
  };

// let of_zipper = _: zipper => failwith("todo Measured.of_zipper");

// type segment = list((measurement, piece))
// and piece =
//   | Whitespace(Whitespace.t)
//   | Grout(Grout.t)
//   // | Shard(Shard.t)
//   | Tile(tile)
// and tile = {
//   id: Id.t,
//   label: Label.t,
//   mold: Mold.t,
//   children: list(segment),
// };

// type siblings = (segment, segment);

// module Anc = {
//   type t = {
//     id: Id.t,
//     label: Label.t,
//     mold: Mold.t,
//     children: ListFrame.t(segment),
//   };
// };

// type generation = ((Layout.measurement, Anc.t), siblings);
// type ancestors = list(generation);

// type relatives = {
//   siblings,
//   ancestors,
// };

// type selection = {
//   focus: Direction.t,
//   content: segment,
// };

// type zipper = {
//   // id_gen: IdGen.t,
//   selection,
//   // backpack: Backpack.t,
//   relatives,
// };

// let shards: segment => list(Shard.t) =
//   List.filter_map(
//     fun
//     | (_, Shard(s)) => Some(s)
//     | _ => None,
//   );

// let split_segment_by_matching_shard = (tile_id: Id.t): (segment => Aba.t(segment, Shard.t)) =>
//   Aba.split(
//     fun
//     | Shard(s) when s.tile_id == tile_id => Either.R(s)
//     | p => L(p)
//   );

// TODO fix weird default
// let rec of_segment = (~origin=1, seg: Segment.t): (int, segment) =>
//   seg
//   |> ListUtil.fold_left_map(
//        (origin, p) => {
//          let (m, p) = of_piece(~origin, p);
//          (m.origin + m.length + 1, (m, p));
//        },
//        origin,
//      )
// and of_piece = (~origin=0, p: Piece.t): (Layout.measurement, piece) =>
//   switch (p) {
//   // TODO(d) change once w includes newlines
//   | Whitespace(w) => ({origin, length: 1}, Whitespace(w))
//   | Grout(g) => ({origin, length: 1}, Grout(g))
//   // | Shard(s) => (
//   //     {origin, length: Unicode.length(Shard.Label.token(s.label))},
//   //     Shard(s),
//   //   )
//   | Tile(t) =>
//     let (hd, tl) = ListUtil.split_first(t.label);
//     let (origin', children) =
//       List.combine(t.children, tl)
//       |> ListUtil.fold_left_map(
//            (origin, (child, token)) => {
//              let (origin, child) = of_segment(~origin, child);
//              (origin + Unicode.length(token) + 1, child);
//            },
//            origin + Unicode.length(hd) + 1,
//          );
//     // TODO -1
//     (
//       {origin, length: origin' - origin - 1},
//       Tile({id: t.id, label: t.label, mold: t.mold, children}),
//     );
//   };

// let of_zipper = _: zipper => failwith("todo Measured.of_zipper");
