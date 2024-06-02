open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Util;

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord, hash)]
  type t = int;
};
include Base;

module Map = Maps.Make(Base);

module Range = {
  [@deriving (show({with_path: false}), sexp, yojson, ord, hash)]
  type t = (Base.t, Base.t);
  let map = Tuples.map2;
};
