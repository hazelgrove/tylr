open Sexplib.Std;
open Util;
open OptUtil.Syntax;

[@deriving sexp]
type t = list(Selem.t);
[@deriving sexp]
type frame = ListFrame.t(Selem.t);

let of_tiles_pat = List.map(t => Selem.Tile(Pat(t)));
let of_tiles_exp = List.map(t => Selem.Tile(Exp(t)));
let of_tiles = List.map(Selem.tile);

let get_tiles = selection =>
  selection
  |> List.map(
       fun
       | Selem.Token(_) => None
       | Tile(tile) => Some(tile),
     )
  |> OptUtil.sequence;
let get_tiles_pat = selection => {
  let* tiles = get_tiles(selection);
  Tiles.get_pat(tiles);
};
let get_tiles_exp = selection => {
  let* tiles = get_tiles(selection);
  Tiles.get_exp(tiles);
};

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

let tip_sorts = (selection: t): option((Sort.t, Sort.t)) => {
  let+ (_, sort_l) = tip(Left, selection)
  and+ (_, sort_r) = tip(Right, selection);
  (sort_l, sort_r);
};

let is_partial =
  List.exists(
    fun
    | Selem.Token(_) => true
    | _ => false,
  );

let is_whole = (s: Sort.t) =>
  List.for_all(
    fun
    | Selem.Tile(Pat(_)) when s == Pat => true
    | Tile(Exp(_)) when s == Exp => true
    | _ => false,
  );

let is_whole_any = selection =>
  is_whole(Pat, selection) || is_whole(Exp, selection);

/*
 let rec is_whole =
   fun
   | []
   | [Selem.Token(_), ..._] => None
   | [Tile(tile), ...selection] => {
       let* tiles = is_whole(selection);
       Tiles.cons(tile, tiles);
     };
 */

let filter_pred = (s: Sort.t) =>
  fun
  | Selem.Tile(tile) when Tile.sort(tile) == s => true
  | _ => false;
let filter_tiles = (s: Sort.t): (t => t) => List.filter(filter_pred(s));
