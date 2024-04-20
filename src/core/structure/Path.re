module Step = {
  include Int;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = int;
};

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Step.t);
  let compare = List.compare(Step.compare);
};
include Base;

module Map = MapUtil.Make(Base);

module Range = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Base.t, Base.t);
  // conservative for non-normalized paths
  let is_empty = ((l, r)) => l == r;
};

module Cursor = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Point
    | Select(Dir.t, Range.t);
  // conservative for non-normalized paths
  let is_point =
    fun
    | Point => true
    | Select(_, rng) => Range.is_empty(rng);
};

module Ghosts = {
  type t = Map.t(Mold.t);
};

module Marks = {
  type t = {
    cursor: option((Base.t, Cursor.t)),
    ghosts: Ghosts.t,
  };
};
