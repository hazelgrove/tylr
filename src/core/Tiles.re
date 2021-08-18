open Sexplib.Std;
open Util;

[@deriving sexp]
type t = list(Tile.t);

let of_pat = List.map(Tile.pat);
let of_exp = List.map(Tile.exp);

let get_pat = ts => ts |> List.map(Tile.get_pat) |> OptUtil.sequence;
let get_exp = ts => ts |> List.map(Tile.get_exp) |> OptUtil.sequence;
