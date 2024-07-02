open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

module T = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = unit;
};
module NT = {
  // two kinds of space NTs: fillable and unfillable.
  // fillable carry whitespace and appear next to convex-tipped tile/grout.
  // unfillable are strictly empty and appear next to space tokens.
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = bool;
};
