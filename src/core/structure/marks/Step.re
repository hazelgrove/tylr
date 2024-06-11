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
};

module Selection = {
  [@deriving (show({with_path: false}), sexp, yojson, hash)]
  type t = Selection.t(Range.t);
  let map = f => Selection.map(Range.map(f));
  let carets: t => (Caret.t, Caret.t) =
    Selection.carets(~split_range=Fun.id);
};

module Cursor = {
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
};
