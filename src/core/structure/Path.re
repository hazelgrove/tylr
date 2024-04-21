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
  let peel = n =>
    fun
    | [hd, ...tl] when n == hd => Some(tl)
    | _ => None;
};
include Base;

module Map = MapUtil.Make(Base);

module Range = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Base.t, Base.t);
  // conservative check for non-normalized paths
  let is_empty = ((l, r)) => l == r;
};

module Cursor = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    // whether this point is focus rather than anchor
    | Point(bool)
    | Select(Dir.t, Range.t);
  // conservative for non-normalized paths
  let is_point =
    fun
    | Point(_) => true
    | Select(_, rng) => Range.is_empty(rng);
};
module Focus = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = option((Base.t, Cursor.t));
  let zip_prefix = (_, _) => failwith("todo");
  let cons = (n, foc: t) =>
    foc |> Option.map(((path, cur)) => ([n, ...path], cur));
  let peel = (n, foc: t) => {
    open OptUtil.Syntax;
    let* (path, cur) = foc;
    let+ path = peel(n, path);
    (path, cur);
  };
  let union = (l: t, r: t) =>
    switch (l, r) {
    | (None, None) => None
    | (Some(_), None)
    | (Some((_, Select(_))), _) => l
    | (None, Some(_))
    | (_, Some((_, Select(_)))) => r
    | (Some((path_l, Point(foc))), Some((path_r, Point(_)))) =>
      let (pre, (path_l, path_r)) = zip_prefix(path_l, path_r);
      Some((pre, Select(foc ? L : R, (path_l, path_r))));
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
    focus: Focus.t,
    ghosts: Ghosts.t,
  };
  let mk = (~focus=?, ~ghosts=Ghosts.empty, ()) => {focus, ghosts};
  let empty = mk();
  let point = foc => mk(~focus=([], Point(foc)), ());
  let put_cursor = (cur, marks) => {...marks, focus: Some(cur)};
  let cons = (n, {focus, ghosts}) => {
    focus: Focus.cons(n, focus),
    ghosts: Ghosts.cons(n, ghosts),
  };
  let peel = (n, {focus, ghosts}) => {
    focus: Focus.peel(n, focus),
    ghosts: Ghosts.peel(n, ghosts),
  };
  let union = (l: t, r: t) => {
    focus: Focus.union(l.focus, r.focus),
    ghosts: Ghosts.union(l.ghosts, r.ghosts),
  };
  let union_all = List.fold_left(union, empty);
};
