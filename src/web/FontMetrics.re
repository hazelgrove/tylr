[@warning "-33"]
open Sexplib.Std;

[@deriving sexp]
type t = {
  row_height: float,
  col_width: float,
};

let init = {row_height: 1., col_width: 1.};
