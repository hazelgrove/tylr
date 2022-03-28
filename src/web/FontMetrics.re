open Sexplib.Std;

[@warning "-33"]
[@deriving (show, sexp)]
type t = {
  row_height: float,
  col_width: float,
};

let init = {row_height: 1., col_width: 1.};
