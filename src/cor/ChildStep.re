open Sexplib.Std;

[@deriving sexp]
type t = int;

let paren_body = 0;

let lam_pat = 0;

let let_pat = 0;
let let_def = 1;
