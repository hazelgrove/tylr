open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

module Int_ppx = {
  include Int;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = int;
};
module Row = Int_ppx;
module Col = Int_ppx;

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    row: Row.t,
    col: Col.t,
  };

  let zero = {row: 0, col: 0};
  let maximum = {row: Int.max_int, col: Int.max_int};

  let compare = (l, r) => {
    let c = Row.compare(l.row, r.row);
    c == 0 ? Col.compare(l.col, r.col) : c;
  };
  let eq = (l, r) => compare(l, r) == 0;
  let lt = (l, r) => compare(l, r) < 0;
  let leq = (l, r) => compare(l, r) <= 0;

  let min = (l, r) => lt(l, r) ? l : r;
  let max = (l, r) => lt(r, l) ? l : r;

  let bound = (~min as l=zero, ~max as r: t, pos: t) => min(max(l, pos), r);

  let shift = (n, loc: t) => {...loc, col: loc.col + n};

  let return = (loc: t, ~ind: Col.t) => {row: loc.row + 1, col: ind};
};
include Base;

module Range = {
  type t = (Base.t, Base.t);
  let empty = pos => (pos, pos);
};

module Caret = {
  include Caret;
  type t = Caret.t(Base.t);
};
module Selection = {
  include Selection;
  type t = Selection.t(Range.t);
  let map_focus: (_, t) => t = Selection.map_focus(~split_range=Fun.id);
  let get_focus: t => Base.t = Selection.get_focus(~split_range=Fun.id);
};
module Cursor = {
  include Cursor;
  type t = Cursor.t(Caret.t, Selection.t);
  let map_focus = (f: Base.t => Base.t) =>
    Cursor.map(Caret.map_focus(f), Selection.map_focus(f));
  let get_point =
    fun
    | Cursor.Point(p) => p
    | Select(_) => raise(Invalid_argument("Layout.Cursor.get_point"));
  let get_focus =
    Cursor.get(Caret.get_focus, s => Some(Selection.get_focus(s)));
};
