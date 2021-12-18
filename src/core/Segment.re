open Sexplib.Std;
open Util;
open OptUtil.Syntax;

[@deriving sexp]
type t = Aba.t(Tile.s, Shard.t);

module Frame = {
  [@deriving sexp]
  type t = Aba.Frame.a(Tile.s, Shard.t);
};

let tip = (d: Direction.t, s: t): option

let connect = (s1: t, s2: t): t =>
  switch ()



module Mold = {
  type segment = t;

  module Sorts = {
    open LR;
    type t = LR.t(Sort.t);
    let merge = (sorts_l: t, sorts_r: t) =>
      {l: sorts_l.l, r: sorts_r.r};
  };

  type t = Mold.t(Sorts.t);

  let merge = Mold.merge(~merge_sorts=Sorts.merge);

  let of_tile = (tile: Tile.Mold.t) => {
    tips: tile.tips,
    sorts: LR.mk(tile.sorts.out, tile.sorts.out),
  };

  let of_shard = ((index, tile): Shard.Mold.t) => {
    let (prefix, suffix) = ListFrame.mk(index, tile.sorts.in);
    let (tip_l, sort_l) =
      switch (prefix) {
      | [] => (tile.tips.l, tile.sorts.out)
      | [sort_l, ..._] => (Right, sort_l)
      };
    let (tip_r, sort_r) =
      switch (suffix) {
      | [] => (tile.tips.r, til.sorts.out)
      | [sort_r, ..._] => (Left, sort_r)
      };
    {
      tips: LR.mk(tip_l, tip_r),
      sorts: LR.mk(sort_l, sort_r),
    };
  };

  let of_ = (segment: segment) => {

  }
};

let empty = ([], []);
let of_tiles = tiles => (tiles, []);

let concat: (t, t) => t = Aba.concat((@));

let tip = (d: Direction.t, segment: t): option(Tip.t) =>
  switch (segment, ListUtil.split_last_opt(segment)) {
  | ([], _)
  | (_, None) => None
  | ([first, ..._], Some((_, last))) =>
    Some(
      switch (d) {
      | Left => Piece.tip(d, first)
      | Right => Piece.tip(d, last)
      },
    )
  };

let tip_sorts = (segment: t): option((Sort.t, Sort.t)) => {
  let+ (_, sort_l) = tip(Left, segment)
  and+ (_, sort_r) = tip(Right, segment);
  (sort_l, sort_r);
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