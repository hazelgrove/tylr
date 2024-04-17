open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = option(Dir.t);

let get = (s, p): t => {
  open OptUtil.Syntax;
  let* s = s;
  let* (a, _) = p < 0 ? None : List.nth_opt(Sort.Map.find(s, Grammar.v), p);
  a;
};
