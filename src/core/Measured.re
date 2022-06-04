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

let rec of_segment =
        (~row=0, ~col=0, ~indent=0, seg: Segment.t): ((int, int), t) =>
  seg
  |> ListUtil.fold_left_map(
       ((row, col), p) => of_piece(~row, ~col, ~indent, p),
       (row, col),
     )
  |> PairUtil.map_snd(union)
and of_piece = (~row=0, ~col=0, ~indent=0, p: Piece.t): ((int, int), t) => {
  let origin: point = {row, col};
  switch (p) {
  | Whitespace({content: c, _} as w) when c == Whitespace.linebreak =>
    // set col to indent
    let last: point = {row: row + 1, col: indent};
    ((row + 1, indent), singleton_w(w, {origin, last}));
  | Whitespace(w) =>
    let last: point = {row, col: col + 1};
    ((row, col + 1), singleton_w(w, {origin, last}));
  | Grout(g) =>
    let last: point = {row, col: col + 1};
    ((row, col + 1), singleton_g(g, {origin, last}));
  | Tile(t) =>
    let (hd, tl) = ListUtil.split_first(t.shards);
    let token = List.nth(t.label);
    let ((row, col), map) =
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
    ((row, col), map |> add_t(t, {
                                    origin,
                                    last: {
                                      row,
                                      col,
                                    },
                                  }));
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

//TODO(andrew): autoformatter: padding
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

let _linearize: measurement => measurement_lin =
  ({origin: {col: origin, _}, last: {col: last, _}}) => {
    origin,
    length: last - origin,
  };

/*
  let get_closed_children = (mold: Mold.t, ms: list(measured)): list(measured) => {
    List.map((==)(mold.sorts.out), mold.sorts.in_)
    |> ListUtil.p_indices((==)(false))
    |> select_piece_idxs(ms);
  };
 */
