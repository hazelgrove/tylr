open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

module T = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = unit;
  let all = [()];
};
module NT = {
  // two kinds of space NTs: fillable and unfillable.
  // fillable carry whitespace and appear next to convex-tipped tile/grout.
  // unfillable are strictly empty and appear next to space tokens.
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = bool;
  let all = [false, true];
};
module Sym = {
  type t = Sym.t(T.t, NT.t);
  let all = List.map(Sym.t, T.all) @ List.map(Sym.nt, NT.all);
};
