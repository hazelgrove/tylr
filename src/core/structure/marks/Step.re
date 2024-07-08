open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord, hash)]
  type t = int;
  let shift = (by, n) => by + n;
};
include Base;

module Map = Maps.Make(Base);

module Range = {
  [@deriving (show({with_path: false}), sexp, yojson, ord, hash)]
  type t = (Base.t, Base.t);
  let map = Tuples.map2;
};

module Caret = {
  include Caret;
  [@deriving (show({with_path: false}), sexp, yojson, hash)]
  type t = Caret.t(Base.t);
  let shift = n => map((+)(n));
};

module Selection = {
  include Selection;
  [@deriving (show({with_path: false}), sexp, yojson, hash)]
  type t = Selection.t(Range.t);
  let map = f => Selection.map(Range.map(f));
  let carets: t => (Caret.t, Caret.t) =
    Selection.carets(~split_range=Fun.id);
  let of_carets = (c1: Caret.t, c2: Caret.t) =>
    Base.compare(c1.path, c2.path) <= 0
      ? mk(~focus=c1.hand == Focus ? L : R, (c1.path, c2.path))
      : mk(~focus=c2.hand == Focus ? L : R, (c2.path, c1.path));
};

module Cursor = {
  include Cursor;
  [@deriving (show({with_path: false}), sexp, yojson, hash)]
  type t = Cursor.t(Caret.t, Selection.t);
  let map = f => Cursor.map(Caret.map(f), Selection.map(f));
  let union = (l: t, r: t) =>
    switch (l, r) {
    | (Select(_), _) => l
    | (_, Select(_)) => r
    | (Point({path: l, hand}), Point({path: r, _})) =>
      Select({focus: hand == Focus ? L : R, range: (l, r)})
    };
  let mk = (c1: Caret.t, c2: Caret.t) =>
    Base.compare(c1.path, c2.path) == 0
      ? Point(Caret.focus(c1.path)) : Select(Selection.of_carets(c1, c2));
};
