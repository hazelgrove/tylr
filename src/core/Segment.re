open Util;

[@deriving sexp]
type t = Aba.t(Tiles.t, Shard.t);

type hd = Tiles.t;
type tl = Baba.t(Shard.t, Tiles.t);

module Piece = {
  [@deriving sexp]
  type t =
    | Shard(Shard.t)
    | Tile(Tile.t)
    | Grout(Tile.Placeholder.t);
};

let cons = (_, _) => failwith("todo Segment.cons");
let snoc = (_, _) => failwith("todo Segment.snoc");
let cons_snoc = (side: Direction.t, piece: Piece.t, segment: t): t =>
  switch (side) {
  | Left => cons(piece, segment)
  | Right => snoc(segment, piece)
  };

let of_pieces = _ => failwith("todo Segment.of_pieces");

let split = (_, _) => failwith("todo Segment.split");

let cons_shard = (_, _) => failwith("todo Segment.cons_shard");
let snoc_shard = (_, _) => failwith("todo Segment.snoc_shard");
let cons_tile = _ => failwith("todo Segment.cons_tile");
let cons_placeholder = _ => failwith("todo Segment.cons_placeholder");

let rev = _ => failwith("todo Segment.rev");

let contains_matching = (_, _) => failwith("todo Segment.contains_matching");
let remove_matching = (_, _) => failwith("todo Segment.remove_matching");

let empty = (Tiles.empty, []);
let of_tiles = tiles => (tiles, []);

let concat: list(t) => t = Util.Aba.concat(Tiles.concat);

let contains = _ => failwith("todo Segment.contains");
let remove = _ => failwith("todo Segment.remove");

let trim = _ => failwith("todo Segment.trim");

let orient = _ => failwith("todo Segment.orient");
let unorient = _ => failwith("todo Segment.unorient");

let is_balanced = _ => failwith("todo Segment.is_balanced");

let fold_left_map = _ => failwith("todo Segment.fold_left_map");

let rec remold = (ctx: Shard.Ctx.t, (l, r): Nibs.t, segment: t): list(t) =>
  switch (segment) {
  | ((ps, []), []) => [
      ((Tiles.Placeholders.adjust((l, r), ps), []), []),
    ]
  | ((ps, []), [(shard, tiles), ...tl]) =>
    open ListUtil.Syntax;
    let* nibs = Shard.nibs(~l, ctx, shard);
    let shard = {...shard, nibs};
    let ctx = Shard.Ctx.add(shard, ctx);
    let ps = Tiles.Placeholders.adjust((l, fst(shard.nibs)), ps);
    let+ (tiles, tl) = remold(ctx, (snd(shard.nibs), r), (tiles, tl));
    ((ps, []), [(shard, tiles), ...tl]);
  | ((ps, [(tile, ps'), ...tiles_tl]), segment_tl) =>
    open ListUtil.Syntax;
    let* mold = Tile.Form.molds(~l, Tile.form(tile));
    let tile = {...tile, mold};
    let nibs = Tile.Mold.nibs(mold);
    let ps = Tiles.Placeholders.adjust((l, fst(nibs)), ps);
    let+ ((ps', tiles_tl), segment_tl) =
      remold(ctx, (snd(nibs), r), ((ps', tiles_tl), segment_tl));
    ((ps, [(tile, ps'), ...tiles_tl]), segment_tl);
  };

let split_hd = (segment: t): option((Piece.t, t)) =>
  switch (segment) {
  | (([], []), []) => None
  | (([], []), [(shard, tiles), ...tl]) =>
    Some((Shard(shard), (tiles, tl)))
  | (([], [(tile, ps), ...tiles_tl]), segment_tl) =>
    Some((Tile(tile), ((ps, tiles_tl), segment_tl)))
  | (([p, ...ps], tiles_tl), segment_tl) =>
    Some((Grout(p), ((ps, tiles_tl), segment_tl)))
  };

module Affix = {
  [@deriving sexp]
  type nonrec t = t;

  let split_hd = split_hd;

  let reshape =
      (d: Direction.t, affix: t, back_nib: Nib.t)
      : (Tiles.hd, list(Tiles.tl), tl) => {
    let (hd, tl) = affix;
    let back_nib =
      switch (tl) {
      | [] => back_nib
      | [(shard, _), ..._] => Direction.(choose(toggle(d), shard.nibs))
      };
    let (tiles_hd, tiles_tls) = Tiles.Frame.reshape_affix(d, hd, back_nib);
    (tiles_hd, tiles_tls, tl);
  };
};

module Frame = {
  type segment = t;
  [@deriving sexp]
  type t = (Affix.t, Affix.t);

  let orient = (d: Direction.t, (prefix, suffix): t): t =>
    switch (d) {
    | Left => (prefix, suffix)
    | Right => (suffix, prefix)
    };
  let unorient = orient;

  let cons = (d: Direction.t, piece: Piece.t, affixes: t): t => {
    let (front, back) = orient(d, affixes);
    unorient(d, (cons(piece, front), back));
  };

  let prepend = (d: Direction.t, segment: segment, affixes: t): t => {
    let segment = d == Left ? rev(segment) : segment;
    let (front, back) = orient(d, affixes);
    unorient(d, (concat([segment, front]), back));
  };

  let split_hd = (d: Direction.t, affixes: t): option((Piece.t, t)) => {
    open OptUtil.Syntax;
    let (front, back) = orient(d, affixes);
    let+ (piece, front) = Affix.split_hd(front);
    (piece, unorient(d, (front, back)));
  };

  let inner_nibs = (_frame, _outer_nibs) =>
    failwith("todo Segment.Frame.inner_nibs");

  let concat = _ => failwith("todo Segment.Frame.concat");

  let reshape = ((prefix, suffix): t, (l, r): Nibs.t) => (
    Affix.reshape(Left, prefix, l),
    Affix.reshape(Right, suffix, r),
  );

  let contains_matching = (_, _) =>
    failwith("todo Segment.Frame.contains_matching");
};

// let trim = (segment: t) => {
//   let trim_l =
//     fun
//     | [hd, ...tl] when Piece.is_hole(hd) => tl
//     | segment => segment;
//   let trim_r = segment =>
//     switch (ListUtil.split_last_opt(segment)) {
//     | Some((leading, last)) when Piece.is_hole(last) => leading
//     | _ => segment
//     };
//   trim_r(trim_l(segment));
// };

// let choose = (_remoldings: list((t, Tile.Frame.s))): (t, Tile.Frame.s) =>
//   failwith("todo");

// let choose: list((t, Frame.t)) => (t, Frame.t) = _ => failwith("todo");

// let connect = (affixes: Frame.t, sort: Sort.t): list(Frame.t) => {
//   let (l, r) = Frame.inner_nibs(affixes, sort);
//   if (l.sort == r.sort) {
//     Nib.of_sort(l.sort)
//     |> List.map(mid => Frame.rerole((mid, mid), affixes, sort))
//     |> List.concat
//   } else {
//     let mid_l = Nib.{sort: l.sort, orientation: Right};
//     let mid_r = Nib.{sort: r.sort, orientation: Left};
//     Frame.rerole((mid_l, mid_r), affixes, sort)
//     |> List.map(((prefix, suffix)) => (prefix, [Tile.Sep, ...suffix]))
//   };
// };

// let insert =
//     (insertion: Segment.t, affixes: Frame.t, sort: Sort.t)
//     : list((Segment.t, Frame.t)) => {
//   open ListUtil.Syntax;
//   let* remolded = remold(insertion, affixes, sort);
//   switch (nibs(remolded)) {
//   | None =>
//     let+ affixes = connect(affixes, sort);
//     (empty, affixes);
//   | Some(inner_nibs) =>
//     let+ affixes = Frame.rerole(inner_nibs, t, sort);

//   };
// };

// let connect =
//     (
//       ~insert: option((Direction.t, t))=?,
//       affixes: Frame.t,
//       outer_nibs: Nibs.t,
//     ) => {
//   // TODO need to trim affixes
//   let insertion =
//     switch (insert) {
//     | None =>
//       let (l, r) = Frame.inner_nibs(affixes, outer_nibs);
//       (empty, Segment.of_tiles(Tiles.glue(l, r)))
//     | Some((d, insertion)) =>
//       let remoldings = remold(insertion, affixes, outer_nibs);
//       let (insertion, (glue_l, glue_r)) = choose(remoldings);
//       let (glue_l, glue_r) = (of_tiles(glue_l), of_tiles(glue_r));
//       switch (d) {
//       | Left =>
//         (glue_l, concat([insertion, glue_r]))
//       | Right =>
//         (concat([rev(insertion), glue_l]), glue_r)
//       };
//     };
//   Frame.append(insertion, affixes);
