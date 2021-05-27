open Sexplib.Std;
open Util;
open OptUtil.Syntax;

[@deriving sexp]
type t = list(Selem.t);
[@deriving sexp]
type frame = ListFrame.t(Selem.t);

let of_tiles_pat = List.map(t => Selem.Tile(Pat(t)));
let of_tiles_exp = List.map(t => Selem.Tile(Exp(t)));
let of_tiles = Tiles.get(of_tiles_pat, of_tiles_exp);

let tip = (d: Direction.t, selection: t): option(Tip.t) =>
  switch (selection, ListUtil.split_last_opt(selection)) {
  | ([], _)
  | (_, None) => None
  | ([first, ..._], Some((_, last))) =>
    Some(
      switch (d) {
      | Left => Selem.tip(d, first)
      | Right => Selem.tip(d, last)
      },
    )
  };

let rec is_whole =
  fun
  | []
  | [Selem.Token(_), ..._] => None
  | [Tile(tile), ...selection] => {
      let* tiles = is_whole(selection);
      Tiles.cons(tile, tiles);
    };

let filter_tiles = (s: Sort.t): (t => t) =>
  List.filter(
    fun
    | Selem.Tile(tile) when Tile.sort(tile) == s => true
    | _ => false,
  );
