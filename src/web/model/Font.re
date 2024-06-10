open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

[@warning "-33"]
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  row_height: float,
  col_width: float,
};

let init = {row_height: 10., col_width: 10.};

let row = (font, row) => Float.of_int(row) *. font.col_width;
let col = (font, col) => Float.of_int(col) *. font.row_height;
