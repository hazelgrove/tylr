open Util;

[@deriving show]
type point = {
  indent: int,
  row: int,
  col: int,
};
let zero = {indent: 0, row: 0, col: 0};

[@deriving show]
type measurement_lin = {
  origin: int,
  length: int,
};

[@deriving show]
type measurement = {
  origin: point,
  last: point,
};

[@deriving show]
type token = {
  row: int,
  indent: int,
  range: (int, int),
};

module Shards = {
  type t = list((int, measurement));

  let last = (shards: t) =>
    shards
    |> List.sort(((i, _), (j, _)) => Int.compare(i, j))
    |> ListUtil.last_opt
    |> Option.map(snd);
};

type t = {
  tiles: Id.Map.t(Shards.t),
  grout: Id.Map.t(measurement),
  whitespace: Id.Map.t(measurement),
};

let empty = {
  tiles: Id.Map.empty,
  grout: Id.Map.empty,
  whitespace: Id.Map.empty,
};

// assumes tile is single shard
let add_t = (t: Tile.t, m, map) => {
  ...map,
  tiles:
    map.tiles
    |> Id.Map.update(
         t.id,
         fun
         | None => Some([(Tile.l_shard(t), m)])
         | Some(ms) => Some([(Tile.l_shard(t), m), ...ms]),
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
let add_p = (p: Piece.t, m, map) =>
  p
  |> Piece.get(
       w => add_w(w, m, map),
       g => add_g(g, m, map),
       t => add_t(t, m, map),
     );

let singleton_w = (w, m) => empty |> add_w(w, m);
let singleton_g = (g, m) => empty |> add_g(g, m);
let singleton_t = (t, m) => empty |> add_t(t, m);

let find_w = (w: Whitespace.t, map) => Id.Map.find(w.id, map.whitespace);
let find_g = (g: Grout.t, map) => Id.Map.find(g.id, map.grout);
let find_t = (t: Tile.t, map) =>
  // assumes t is single shard
  List.assoc(Tile.l_shard(t), Id.Map.find(t.id, map.tiles));
// let find_a = ({shards: (l, r), _} as a: Ancestor.t, map) =>
//   List.assoc(l @ r, Id.Map.find(a.id, map.tiles));
let find_p = (p: Piece.t, map) =>
  p
  |> Piece.get(
       w => find_w(w, map),
       g => find_g(g, map),
       t => find_t(t, map),
     );

// TODO(d) rename
let find_opt_shards = (t: Tile.t, map) => Id.Map.find_opt(t.id, map.tiles);

let union2 = (map: t, map': t) => {
  tiles:
    Id.Map.union((_, ms, ms') => Some(ms @ ms'), map.tiles, map'.tiles),
  grout: Id.Map.union((_, m, _) => Some(m), map.grout, map'.grout),
  whitespace:
    Id.Map.union((_, m, _) => Some(m), map.whitespace, map'.whitespace),
};
let union = List.fold_left(union2, empty);

let rec of_segment' =
        (~prev: Segment.t, ~origin: point, ~map: t, ~next: Segment.t)
        : (Segment.t, point, t) => {
  let rec peek: Segment.t => option(Tile.t) =
    fun
    | [] => None
    | [Tile(t), ..._] => Some(t)
    | [_, ...tl] => peek(tl);
  open OptUtil.Syntax; // returns Some(indent) if next has container shard

  let prev_m = peek(prev) |> Option.map(t => find_t(t, map));
  let prev_indent =
    switch (peek(prev)) {
    | None => false
    | Some(t) => Tile.r_shard(t) != List.length(t.label) - 1
    };
  let next_dedent =
    switch (peek(next)) {
    | None => false
    | Some(t) => Tile.l_shard(t) != 0
    };
  let next_matching: option(int) = {
    let* t = peek(next);
    let+ shards = find_opt_shards(t, map);
    let m = Shards.last(shards) |> OptUtil.get_or_fail("expected non-empty");
    m.last.indent;
  };
  let next_indent =
    switch (next_matching) {
    | Some(i) => i
    | None =>
      let prev_origin_col =
        switch (prev_m) {
        | None => 0
        | Some(m) => m.origin.col
        };
      if (prev_indent && next_dedent) {
        origin.indent;
      } else if (prev_indent) {
        origin.indent + 2;
      } else if (next_dedent) {
        origin.indent - 2;
      } else {
        prev_origin_col;
      };
    };
  let len =
    fun
    | Piece.Whitespace(_)
    | Grout(_) => 1
    | Tile(t) => String.length(List.nth(t.label, Tile.l_shard(t)));
  switch (next) {
  | [] => (prev, origin, map)
  | [p, ...next] =>
    let last =
      switch (p) {
      | Whitespace(w) when w.content == Whitespace.linebreak => {
          row: origin.row + 1,
          col: next_indent,
          indent: next_indent,
        }
      | _ => {...origin, col: origin.col + len(p)}
      };
    let map = add_p(p, {origin, last}, map);
    of_segment'(~prev=[p, ...prev], ~next, ~origin=last, ~map);
  };
};
let of_segment = (seg: Segment.t) => {
  let (_, _, map) =
    of_segment'(~prev=[], ~origin=zero, ~next=seg, ~map=empty);
  map;
};

// let rec of_segment =
//         (~row=0, ~col=0, ~indent=0, seg: Segment.t): ((int, int), t) =>
//   seg
//   |> ListUtil.fold_left_map(
//        ((row, col), p) => of_piece(~row, ~col, ~indent, p),
//        (row, col),
//      )
//   |> PairUtil.map_snd(union)
// and of_piece = (~row=0, ~col=0, ~indent=0, p: Piece.t): ((int, int), t) => {
//   let origin: point = {row, col};
//   switch (p) {
//   | Whitespace({content: c, _} as w) when c == Whitespace.linebreak =>
//     // set col to indent
//     let last: point = {row: row + 1, col: indent};
//     ((row + 1, indent), singleton_w(w, {origin, last}));
//   | Whitespace(w) =>
//     let last: point = {row, col: col + 1};
//     ((row, col + 1), singleton_w(w, {origin, last}));
//   | Grout(g) =>
//     let last: point = {row, col: col + 1};
//     ((row, col + 1), singleton_g(g, {origin, last}));
//   | Tile(t) =>
//     let (hd, tl) = ListUtil.split_first(t.shards);
//     let token = List.nth(t.label);
//     let ((row, col), map) =
//       List.combine(t.children, tl)
//       |> ListUtil.fold_left_map(
//            ((row, col), (child, i)) => {
//              let ((row, col), map) =
//                of_segment(~row, ~col, ~indent=indent + 1, child);
//              ((row, col + String.length(token(i))), map);
//            },
//            (row, col + String.length(token(hd))),
//          )
//       |> PairUtil.map_snd(union);
//     ((row, col), map |> add_t(t, {
//                                     origin,
//                                     last: {
//                                       row,
//                                       col,
//                                     },
//                                   }));
//   };
// };

let length = (seg: Segment.t, map: t): int =>
  switch (seg) {
  | [] => 0
  | [p] =>
    let m = find_p(p, map);
    m.last.col - m.origin.col;
  | [hd, ...tl] =>
    let first = find_p(hd, map);
    let last = find_p(ListUtil.last(tl), map);
    last.last.col - first.origin.col;
  };

let relativize_measurements:
  (int, list(measurement_lin)) => list(measurement_lin) =
  parent_origin =>
    List.map(({origin, length}) =>
      {origin: origin - parent_origin, length}
    );

let _linearize: measurement => measurement_lin =
  ({origin: {col: origin, _}, last: {col: last, _}}) => {
    origin,
    length: last - origin,
  };

let segment_origin = (seg: Segment.t): option(point) =>
  Option.map(
    first => find_p(first, of_segment(seg)).origin,
    ListUtil.hd_opt(seg),
  );

let segment_last = (seg: Segment.t): option(point) =>
  Option.map(
    last => find_p(last, of_segment(seg)).last,
    ListUtil.last_opt(seg),
  );

let segment_height = (seg: Segment.t) =>
  switch (segment_last(seg), segment_origin(seg)) {
  | (Some(last), Some(first)) => 1 + last.row - first.row
  | _ => 0
  };

let segment_width = (seg: Segment.t): int => {
  let map = of_segment(seg);
  List.fold_left(
    (acc, p: Piece.t) => max(acc, find_p(p, map).last.col),
    0,
    seg,
  );
};
