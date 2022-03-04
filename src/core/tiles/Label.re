open Sexplib.Std;

[@deriving sexp]
type t = list(Token.t);

let len: t => int = List.length;

exception Empty_label;
