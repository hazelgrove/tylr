open Sexplib.Std;

[@deriving sexp]
type t = string;

let compare = String.compare;
