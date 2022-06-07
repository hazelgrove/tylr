open Util;

[@deriving show]
type point = {
  // indent: int,
  row: int,
  col: int,
};
let zero = {/*indent: 0,*/ row: 0, col: 0};

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

module Rows = {
  include IntMap;
  type shape = {
    indent: int,
    max_col: int,
  };
  type t = IntMap.t(shape);
};

module Shards = {
  type shard = (int, measurement);
  type t = list(shard);

  // elements of returned list are nonempty
  let rec split_by_row: t => list(t) =
    fun
    | [] => []
    | [hd, ...tl] =>
      switch (split_by_row(tl)) {
      | [] => [[hd]]
      | [row, ...rows] =>
        snd(List.hd(row)).origin.row == snd(hd).origin.row
          ? [[hd, ...row], ...rows] : [[hd], row, ...rows]
      };
  // let last = (shards: t) =>
  //   shards
  //   |> List.sort(((i, _), (j, _)) => Int.compare(i, j))
  //   |> ListUtil.last_opt
  //   |> Option.map(snd);
};

type t = {
  tiles: Id.Map.t(Shards.t),
  grout: Id.Map.t(measurement),
  whitespace: Id.Map.t(measurement),
  rows: Rows.t,
};

let empty = {
  tiles: Id.Map.empty,
  grout: Id.Map.empty,
  whitespace: Id.Map.empty,
  rows: Rows.empty,
};

let add_s = (id: Id.t, i: int, m, map) => {
  ...map,
  tiles:
    map.tiles
    |> Id.Map.update(
         id,
         fun
         | None => Some([(i, m)])
         | Some(ms) =>
           Some(
             [(i, m), ...ms]
             |> List.sort(((i, _), (j, _)) => Int.compare(i, j)),
           ),
       ),
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

let add_row = (row: int, shape: Rows.shape, map) => {
  ...map,
  rows: Rows.add(row, shape, map.rows),
};

let singleton_w = (w, m) => empty |> add_w(w, m);
let singleton_g = (g, m) => empty |> add_g(g, m);
let singleton_s = (id, shard, m) => empty |> add_s(id, shard, m);

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
let find_shards = (t: Tile.t, map) => Id.Map.find(t.id, map.tiles);

let find_shards' = (id: Id.t, map) =>
  switch (Id.Map.find_opt(id, map.tiles)) {
  | None => []
  | Some(ss) => ss
  };

let union2 = (map: t, map': t) => {
  tiles:
    Id.Map.union((_, ms, ms') => Some(ms @ ms'), map.tiles, map'.tiles),
  grout: Id.Map.union((_, m, _) => Some(m), map.grout, map'.grout),
  whitespace:
    Id.Map.union((_, m, _) => Some(m), map.whitespace, map'.whitespace),
  rows: Rows.union((_, s, _) => Some(s), map.rows, map'.rows),
};
let union = List.fold_left(union2, empty);

let rec of_segment =
        (~seen_linebreak=false, ~indent=0, ~origin=zero, seg: Segment.t)
        : (point, t) =>
  switch (seg) {
  | [] => (origin, empty)
  | [hd, ...tl] =>
    let (seen_linebreak, indent, hd_last, hd_map) =
      switch (hd) {
      | Whitespace(w) when w.content == Whitespace.linebreak =>
        let concluding = Segment.sameline_whitespace(tl);
        let indent' =
          if (!seen_linebreak && concluding) {
            indent;
          } else if (!seen_linebreak) {
            indent + 2;
          } else if (concluding) {
            indent - 2;
          } else {
            indent;
          };
        let last = {row: origin.row + 1, col: indent'};
        let map =
          singleton_w(w, {origin, last})
          |> add_row(origin.row, {indent, max_col: origin.col});
        (true, indent', last, map);
      | Whitespace(w) =>
        let last = {...origin, col: origin.col + 1};
        (seen_linebreak, indent, last, singleton_w(w, {origin, last}));
      | Grout(g) =>
        let last = {...origin, col: origin.col + 1};
        (seen_linebreak, indent, last, singleton_g(g, {origin, last}));
      | Tile(t) =>
        let token = List.nth(t.label);
        let of_shard = (origin, shard) => {
          let last = {
            ...origin,
            col: origin.col + String.length(token(shard)),
          };
          (last, singleton_s(t.id, shard, {origin, last}));
        };
        let (last, map) =
          Aba.mk(t.shards, t.children)
          |> Aba.fold_left_map(
               of_shard(origin),
               (origin, child, shard) => {
                 let (child_last, child_map) =
                   of_segment(~indent, ~origin, child);
                 let (shard_last, shard_map) = of_shard(child_last, shard);
                 (shard_last, child_map, shard_map);
               },
             )
          |> PairUtil.map_snd(Aba.join(Fun.id, Fun.id))
          |> PairUtil.map_snd(union);
        (seen_linebreak, indent, last, map);
      };
    let (tl_last, tl_map) =
      of_segment(~seen_linebreak, ~indent, ~origin=hd_last, tl);
    (tl_last, union2(hd_map, tl_map));
  };

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
    first => find_p(first, snd(of_segment(seg))).origin,
    ListUtil.hd_opt(seg),
  );

let segment_last = (seg: Segment.t): option(point) =>
  Option.map(
    last => find_p(last, snd(of_segment(seg))).last,
    ListUtil.last_opt(seg),
  );

let segment_height = (seg: Segment.t) =>
  switch (segment_last(seg), segment_origin(seg)) {
  | (Some(last), Some(first)) => 1 + last.row - first.row
  | _ => 0
  };

let segment_width = (seg: Segment.t): int => {
  let (_, map) = of_segment(seg);
  List.fold_left(
    (acc, p: Piece.t) => max(acc, find_p(p, map).last.col),
    0,
    seg,
  );
};
