open Sexplib.Std;

[@deriving sexp]
type t = list(Ancestor.t);

let empty = [];

let sort: t => Sort.t =
  fun
  | [] => Sort.Exp
  | [(tile_frame, _), ..._] => Tile.Frame.sort(tile_frame);
