open Sexplib.Std;
open Util;

[@deriving sexp]
type t = Tile.s;

let empty = [];

let rev = List.rev;
