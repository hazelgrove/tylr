open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(Terr.t);

let empty = [];
let singleton = t => [t];
// let of_piece = p => of_terr(Terr.of_piece(p));
let height = List.length;

let face =
  fun
  | [] => None
  | [hd, ..._] => Some(Terr.face(hd));

let extend = tl =>
  fun
  | [] => None
  | [hd, ...rest] => Some([Terr.extend(tl, hd), ...rest]);

let fold: (('acc, Terr.t) => 'acc, 'acc, t) => 'acc = List.fold_left;

let cat = (@);

// Dn and Up slopes named based on left-to-right order of terraces
// as displayed on screen, but terraces are always maintained
// in list order low-to-high
module Dn = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Terr.R.t);
};
module Up = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Terr.L.t);
};
