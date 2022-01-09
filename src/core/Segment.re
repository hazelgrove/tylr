open Sexplib.Std;
open Util;
open OptUtil.Syntax;

[@deriving sexp]
type t = Aba.t(Tile.s, Nibbed.t(Shard.t));

let empty = ([], []);
let of_tiles = tiles => (tiles, []);

let concat: (t, t) => t = Aba.concat((@));

module Frame = {
  [@deriving sexp]
  type t = Aba.Frame.a(Tile.s, Nibbed.t(Shard.t));
};

// produce possible front nibs such that
// segment connects front nib to back nib
let rec syn = (
  back: Direction.t,
  segment: t,
  back_nib: Nib.t,
): list(Nib.t) => {
  let front = Direction.toggle(d);
  switch (segment) {
  | ([], []) => [back_nib]
  | ([], [((nibs, _shard), _tiles), ..._]) =>
    [Direction.(choose(front, nibs))]
  | ([tile, ...tiles], tl) =>
    Tile.molds(tile)
    |> List.filter_map(
      mold => {
        let nibs = Tile.Mold.nibs(mold);
        let front_nib = Direction.choose(back, nibs);
        ana(d, front_nib, (tiles, tl), back_nib)
        ? None
        : Some(Direction.choose(front, nibs))
      }
    )
  };
}
// check if segment connects front nib to back nib
and ana = (
  back: Direction.t,
  front_nib: Nib.t,
  segment: t,
  back_nib: Nib.t,
): bool =>
  syn(back, segment, back_nib)
  |> List.exists((==)(front_nib));

let glue = (l: Nib.t, r: Nib.t): Tile.s => {
  let hole_nibs =
  if (l == r) {
    []
  } else if (l.sort == r.sort) {
    [(l, r)]
  } else {
    let nibs =
    switch (l.orientation, r.orientation) {
    | (Left, Right) =>
      let (l', r') = (Nib.toggle(l), Nib.toggle(r));
      [(l, l'), (l', r'), (r', r)];
    | (Left, Left) =>
      let l' = Nib.toggle(l);
      [(l, l'), (l', r)];
    | (Right, Right) =>
      let r' = Nib.toggle(r);
      [(l, r'), (r', r)];
    | (Right, Left) => [(l, r)]
    };
  };
  Tile.s_of_nibs(hole_nibs);
};

let connect = (l: Nib.t, pre: t, suf: t, r: Nib.t): t => {
  let nibs_pre = syn(Left, trim_hd(pre), l);
  let nibs_suf = syn(Right, trim_hd(suf), r);
  let glue =
    ListUtil.prod(nibs_pre, nibs_suf)
    |> List.map(((l, r)) => glue(l, r))
    |> List.map(Tile.s_len)
    |> List.sort(Int.compare)
    // shouldn't fail so long as every tile has at least one mold
    |> List.hd;
  concat([pre, of_tiles(glue), suf]);
};

let is_cracked =
  List.exists(
    fun
    | Piece.Shard(_) => true
    | _ => false,
  );

let is_intact = (s: Sort.t) =>
  List.for_all(
    fun
    | Piece.Tile(Pat(_)) when s == Pat => true
    | Tile(Exp(_)) when s == Exp => true
    | _ => false,
  );

let is_intact_any = segment =>
  is_intact(Pat, segment) || is_intact(Exp, segment);

/*
 let rec is_whole =
   fun
   | []
   | [Piece.Shard(_), ..._] => None
   | [Tile(tile), ...segment] => {
       let* tiles = is_whole(segment);
       Tiles.cons(tile, tiles);
     };
 */

let filter_pred = (s: Sort.t) =>
  fun
  | Piece.Tile(tile) when Tile.sort(tile) == s => true
  | _ => false;
let filter_tiles = (s: Sort.t): (t => t) => List.filter(filter_pred(s));

let has_same_sort_caps = (segment: t): option(Sort.t) =>
  switch (tip(Left, segment), tip(Right, segment)) {
  | (Some((_, sort_l)), Some((_, sort_r))) when sort_l == sort_r =>
    Some(sort_l)
  | _ => None
  };

let trim_end_holes = (segment: Segment.t) => {
  let trim_l =
    fun
    | [hd, ...tl] when Piece.is_hole(hd) => tl
    | segment => segment;
  let trim_r = segment =>
    switch (ListUtil.split_last_opt(segment)) {
    | Some((leading, last)) when Piece.is_hole(last) => leading
    | _ => segment
    };
  trim_r(trim_l(segment));
}