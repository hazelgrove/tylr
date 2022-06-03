open Util;

[@deriving show]
type point = {
  row: int,
  col: int,
};

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

type t = {
  tiles: Id.Map.t(list((list(int), measurement))),
  grout: Id.Map.t(measurement),
  whitespace: Id.Map.t(measurement),
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
let find_a = ({shards: (l, r), _} as a: Ancestor.t, map) =>
  List.assoc(l @ r, Id.Map.find(a.id, map.tiles));
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

/*
 let rec to_measured2d = (~row=0, ~col=0, ~indent=0, layout: t): measured =>
   switch (layout) {
   | Atom(s, ann) =>
     let measurement =
       mk_measure2d(
         ~row,
         ~col,
         ~last_row=row,
         ~last_col=col + Unicode.length(s),
       );
     {layout: AtomM(s, ann), measurement};
   | Cat(ls, ann) =>
     let (ms, (last_row, last_col)) =
       List.fold_left(
         ((ms, (row, col)), l) => {
           switch (l) {
           | Cat(_empty, Piece(Whitespace("\n"), _, _)) =>
             //let indent = indent + 2;
             let m = to_measured2d(l, ~row, ~col, ~indent); //display at end of current line
             ([m, ...ms], (row + 1, indent + indent_piece)); //reset col, incr row
           //TODO(andrew): why + indent_piece necessary??
           | Cat(_empty, Segment(_)) =>
             let m =
               to_measured2d(l, ~row, ~col, ~indent=indent + indent_segment); //incr indent recurse
             //TODO(andrew): below math dont make sense
             (
               [m, ...ms],
               (m.measurement.d2.last.row, m.measurement.d2.last.col),
             );
           | Cat(_empty, Piece(_)) =>
             let m =
               to_measured2d(l, ~row, ~col, ~indent=indent + indent_piece); //incr indent recurse
             //TODO(andrew): below math dont make sense
             (
               [m, ...ms],
               (m.measurement.d2.last.row, m.measurement.d2.last.col),
             );
           | Atom(_) =>
             let m = to_measured2d(l, ~row, ~col, ~indent);
             //TODO(andrew): below math dont make sense
             (
               [m, ...ms],
               (m.measurement.d2.last.row, m.measurement.d2.last.col),
             );
           }
         },
         ([], (row, col)),
         ls,
       );
     let measurement = mk_measure2d(~row, ~col, ~last_row, ~last_col);
     {layout: CatM(List.rev(ms), ann), measurement};
   };

  */

// TODO fix weird default
let rec of_segment =
        (~row=0, ~col=0, ~indent=0, seg: Segment.t): ((int, int), t) =>
  seg
  |> ListUtil.fold_left_map(
       ((row, col), p) => of_piece(~row, ~col, ~indent, p),
       (row, col),
     )
  |> PairUtil.map_snd(union)
//  (m.origin + m.length + 1, (m, p));
and of_piece = (~row=0, ~col=0, ~indent=0, p: Piece.t): ((int, int), t) => {
  let singl: measurement = {
    let origin: point = {row, col};
    let last: point = {row, col: col + 1};
    {origin, last};
  };
  switch (p) {
  | Whitespace({content: c, _} as w) when c == Whitespace.linebreak =>
    // set col to indent
    ((row + 1, indent), singleton_w(w, singl))
  | Whitespace(w) => ((row, col + 1), singleton_w(w, singl))
  | Grout(g) => ((row, col + 1), singleton_g(g, singl))
  | Tile(t) =>
    let origin: point = {row, col};
    //let indent' = indent + 1; //TODO(andrew): incr indent
    let (hd, tl) = ListUtil.split_first(t.shards);
    let token = List.nth(t.label);
    let ((row', col'), map) =
      List.combine(t.children, tl)
      |> ListUtil.fold_left_map(
           ((row, col), (child, i)) => {
             let ((row, col), map) =
               of_segment(~row, ~col, ~indent=indent + 1, child);
             ((row, col + String.length(token(i))), map);
           },
           (row, col + String.length(token(hd))),
         )
      |> PairUtil.map_snd(union);
    let last: point = {row: row', col: col'};
    ((row', col'), map |> add_t(t, {origin, last}));
  };
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

// let of_zipper = _: zipper => failwith("todo of_zipper");

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

// type generation = ((measurement, Anc.t), siblings);
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
// and of_piece = (~origin=0, p: Piece.t): (measurement, piece) =>
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

// let of_zipper = _: zipper => failwith("todo of_zipper");

//TODO(andrew): padding?
[@deriving show]
type padding =
  | None
  | Bi
  | Pre
  | Post;

let padding: string => padding =
  fun
  | "fun"
  | "let" => Post
  | "=>"
  | "+"
  | "-"
  | "*"
  | "/"
  | ","
  | "="
  | "in"
  | "?"
  | ":" => Bi
  | "("
  | ")"
  | "["
  | "]"
  | "}"
  | _ => None;

let relativize_measurements:
  (int, list(measurement_lin)) => list(measurement_lin) =
  parent_origin =>
    List.map(({origin, length}) =>
      {origin: origin - parent_origin, length}
    );

let linearize: measurement => measurement_lin =
  ({origin: {col: origin, _}, last: {col: last, _}}) => {
    origin,
    length: last - origin,
  };

/*
  let rec to_measured = (~origin=0, layout: t): measured =>
    switch (layout) {
    | Atom(t, ann) =>
      let measurement = {origin, length: token_length(t)};
      {layout: AtomM(t, ann), measurement};
    | Cat(ls, ann) =>
      let (ms, final) =
        List.fold_left(
          ((ms, origin), l) => {
            let m = to_measured(l, ~origin);
            ([m, ...ms], origin + m.measurement.length);
          },
          ([], origin),
          ls,
        );
      let measurement = {origin, length: final - origin};
      {layout: CatM(List.rev(ms), ann), measurement};
    };

  let select_piece_idxs = xs =>
    List.map(idx => {
      // NOTE: This re-indexing is because of delims, NOT padding
      let i = 2 * idx + 1;
      assert(i >= 0 && i < List.length(xs));
      List.nth(xs, i);
    });

  let get_closed_children = (mold: Mold.t, ms: list(measured)): list(measured) => {
    List.map((==)(mold.sorts.out), mold.sorts.in_)
    |> ListUtil.p_indices((==)(false))
    |> select_piece_idxs(ms);
  };

  let get_open_children = (mold: Mold.t, ms: list(measured)): list(measured) => {
    List.map((==)(mold.sorts.out), mold.sorts.in_)
    |> ListUtil.p_indices((==)(true))
    |> select_piece_idxs(ms);
  };
 */
