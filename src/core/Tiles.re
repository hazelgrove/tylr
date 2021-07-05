open Sexplib.Std;
open Util;

[@deriving sexp]
type t = list(Tile.t);

let of_pat = List.map(Tile.pat);
let of_exp = List.map(Tile.exp);

let get_pat = ts => ts |> List.map(Tile.get_pat) |> OptUtil.sequence;
let get_exp = ts => ts |> List.map(Tile.get_exp) |> OptUtil.sequence;
/*
 let get = (get_pat, get_exp) =>
   fun
   | Pat(ts) => get_pat(ts)
   | Exp(ts) => get_exp(ts);

 let sort = get(_ => Sort.Pat, _ => Sort.Exp);

 let nil =
   fun
   | Sort.Pat => Pat([])
   | Exp => Exp([]);

 let cons = (tile: Tile.t, tiles: t) =>
   switch (tile, tiles) {
   | (Pat(tile), Pat(tiles)) => Some(Pat([tile, ...tiles]))
   | (Exp(tile), Exp(tiles)) => Some(Exp([tile, ...tiles]))
   | _ => None
   };
 */
