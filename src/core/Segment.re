open Util;

[@deriving sexp]
type t = Aba.t(Tiles.t, Shard.t);

[@deriving sexp]
type hd = Tiles.t;
[@deriving sexp]
type tl = Baba.t(Shard.t, Tiles.t);

module Piece = {
  [@deriving sexp]
  type t =
    | Shard(Shard.t)
    | Tile(Tile.t)
    | Grout(Grout.t);
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
let cons_grout = _ => failwith("todo Segment.cons_grout");

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
  [@deriving sexp]
  type nonrec tl = tl;

  let split_hd = split_hd;
  // let reshape =
  //     (d: Direction.t, affix: t, back_nib: Nib.t)
  //     : (Tiles.hd, list(Tiles.tl), tl) => {
  //   let (hd, tl) = affix;
  //   let back_nib =
  //     switch (tl) {
  //     | [] => back_nib
  //     | [(shard, _), ..._] => Direction.(choose(toggle(d), shard.nibs))
  //     };
  //   let (tiles_hd, tiles_tls) = Tiles.Affix.reshape(d, hd, back_nib);
  //   (tiles_hd, tiles_tls, tl);
  // };
};

module Frame = {
  type segment = t;
  [@deriving sexp]
  type t = (Affix.t, Affix.t);

  let of_grouts = (_: Grouts.Frame.t): t =>
    failwith("Segment.Frame.of_grouts");
  let of_tiles = (_: Tiles.Frame.t): t => failwith("Segment.Frame.of_tiles");

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

  // let reshape = ((prefix, suffix): t, (l, r): Nibs.t) => (
  //   Affix.reshape(Left, prefix, l),
  //   Affix.reshape(Right, suffix, r),
  // );

  let near_nibs_tls = (_, _) => failwith("todo Segment.Frame.near_nibs_tls");

  let contains_matching = (_, _) =>
    failwith("todo Segment.Frame.contains_matching");

  let fill = (_: segment, _: t): segment =>
    failwith("todo Segment.Frame.fill");
};

let trim_grouts = (_: t): (t, Grouts.Frame.t) =>
  failwith("todo Segment.trim_grouts");

let rec remold = (segment: t, (l, r): Nibs.t, ctx: Shard.Ctx.t): list(t) =>
  switch (segment) {
  | ((ps, []), []) => [((Grouts.adjust((l, r), ps), []), [])]
  | ((ps, []), [(shard, tiles), ...tl]) =>
    open ListUtil.Syntax;
    let* nibs = Shard.nibs(~l, ctx, shard);
    let shard = {...shard, nibs};
    let ctx = Shard.Ctx.add(shard, ctx);
    let ps = Grouts.adjust((l, fst(shard.nibs)), ps);
    let+ (tiles, tl) = remold((tiles, tl), (snd(shard.nibs), r), ctx);
    ((ps, []), [(shard, tiles), ...tl]);
  | ((ps, [(tile, ps'), ...tiles_tl]), segment_tl) =>
    open ListUtil.Syntax;
    let* mold = Tile.Form.molds(~l, Tile.form(tile));
    let tile = {...tile, mold};
    let nibs = Tile.Mold.nibs(mold);
    let ps = Grouts.adjust((l, fst(nibs)), ps);
    let+ ((ps', tiles_tl), segment_tl) =
      remold(((ps', tiles_tl), segment_tl), (snd(nibs), r), ctx);
    ((ps, [(tile, ps'), ...tiles_tl]), segment_tl);
  };

let remold_reshape =
    (insertion: t, affixes: Tiles.Frame.t, nibs: Nibs.t, ctx: Shard.Ctx.t)
    : list((t, Grouts.Frame.t, Tiles.Frame.t)) => {
  open ListUtil.Syntax;
  let* tiles_frame = Tiles.Frame.reshape(affixes, nibs);
  let (grouts, nibs) = Tiles.Frame.near_nibs(tiles_frame, nibs);
  let+ insertion =
    remold(Frame.(fill(insertion, of_grouts(grouts))), nibs, ctx);
  let (insertion, grouts) = trim_grouts(insertion);
  (insertion, grouts, tiles_frame);
};

let connect =
    // ~focus=Direction.Right,
    (insertion: t, affixes: Frame.t, s: Sort.t): (t, Frame.t) => {
  let ctx = failwith("todo ctx");
  let ((tiles_l, prefix_tl), (tiles_r, suffix_tl)) = affixes;
  let nibs = Frame.near_nibs_tls((prefix_tl, suffix_tl), Nibs.of_sort(s));
  let (insertion, grouts, tiles) =
    remold_reshape(insertion, (tiles_l, tiles_r), nibs, ctx)
    |> List.sort(((_, grouts, _), (_, grouts', _)) =>
         Grouts.Frame.(Int.compare(size(grouts), size(grouts')))
       )
    |> ListUtil.hd_opt
    |> OptUtil.get_or_fail("Segment.insert: expected at least one remolding");
  (insertion, Frame.(concat([of_grouts(grouts), of_tiles(tiles)])));
};
