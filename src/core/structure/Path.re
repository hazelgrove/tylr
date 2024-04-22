open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Util;

exception Invalid;

module Step = {
  include Int;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = int;
};

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Step.t);
  let compare = List.compare(Step.compare);
  let cons = List.cons;
  let peel = n =>
    fun
    | [hd, ...tl] when n == hd => Some(tl)
    | _ => None;
};
include Base;

module Map = MapUtil.Make(Base);

module Point = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type is_focus = bool;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    is_focus,
    path: Base.t,
  };
  let cons = (n, p: t) => {...p, path: Base.cons(n, p.path)};
  let peel = (n, p: t) =>
    Base.peel(n, p.path) |> Option.map(path => {...p, path});
};

module Range = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Base.t, Base.t);
  // conservative check for non-normalized paths
  let is_empty = ((l, r)) => l == r;
  let cons = (n, (l, r)) => Base.(cons(n, l), cons(n, r));
  let peel = (n, (l, r)) => {
    open OptUtil.Syntax;
    let+ l = Base.peel(n, l)
    and+ r = Base.peel(n, r);
    (l, r);
  };
};

module Cursor = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = option(Cursor.t(Point.t, Range.t));
  let cons = n => Option.map(Cursor.map(Point.cons(n), Range.cons(n)));
  let peel = (n, cur) =>
    Option.bind(
      cur,
      fun
      | Cursor.Point(p) => Option.map(Cursor.point, Point.peel(n, p))
      | Select(d, r) => Option.map(Cursor.select(d), Range.peel(n, r)),
    );
  let union = (l: t, r: t) =>
    switch (l, r) {
    | (None, None) => None
    | (Some(_), None)
    | (Some(Select(_)), _) => l
    | (None, Some(_))
    | (_, Some(Select(_))) => r
    | (Some(Point(l)), Some(Point(r))) =>
      Some(Select(l.is_focus ? L : R, (l.path, r.path)))
    };
};

module Ghosts = {
  include Map;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Map.t(Mold.t);
  let to_list = bindings;
  let of_list = bindings => of_seq(List.to_seq(bindings));
  let cons = (n, ghosts) =>
    to_list(ghosts)
    |> List.rev_map(((path, mold)) => ([n, ...path], mold))
    |> of_list;
  let peel = (n, ghosts) =>
    to_list(ghosts)
    |> List.filter_map(((path, mold)) =>
         switch (path) {
         | [m, ...ms] when m == n => Some((ms, mold))
         | _ => None
         }
       )
    |> of_list;
  let union = union((_, m, _) => Some(m));
};

module Marks = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    cursor: Cursor.t,
    ghosts: Ghosts.t,
  };
  let mk = (~cursor=?, ~ghosts=Ghosts.empty, ()) => {cursor, ghosts};
  let empty = mk();
  let point = is_focus => mk(~cursor=Point(Point.{is_focus, path: []}), ());
  let put_cursor = (cur, marks) => {...marks, cursor: Some(cur)};
  let cons = (n, {cursor, ghosts}) => {
    cursor: Cursor.cons(n, cursor),
    ghosts: Ghosts.cons(n, ghosts),
  };
  let peel = (n, {cursor, ghosts}) => {
    cursor: Cursor.peel(n, cursor),
    ghosts: Ghosts.peel(n, ghosts),
  };
  let union = (l: t, r: t) => {
    cursor: Cursor.union(l.cursor, r.cursor),
    ghosts: Ghosts.union(l.ghosts, r.ghosts),
  };
  let union_all = List.fold_left(union, empty);
};
