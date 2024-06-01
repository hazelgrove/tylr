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

module Map = Maps.Make(Base);

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

module Select = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    focus: Base.t,
    anchor: Base.t,
  };
  // conservative check for non-normalized paths
  let is_empty = (sel: t) => Base.compare(sel.focus, sel.anchor) == 0;
  let order = (sel: t) =>
    Base.compare(sel.focus, sel.anchor) <= 0
      ? (Dir.L, (sel.focus, sel.anchor)) : (R, (sel.anchor, sel.focus));
  let cons = (n, sel) => {
    focus: Base.cons(n, sel.focus),
    anchor: Base.cons(n, sel.anchor),
  };
  let peel = (n, sel) => {
    open Options.Syntax;
    let+ focus = Base.peel(n, sel.focus)
    and+ anchor = Base.peel(n, sel.anchor);
    {focus, anchor};
  };
};

module Cursor = {
  include Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Cursor.t(Point.t, Select.t);
  let cons = n => Option.map(map(Point.cons(n), Select.cons(n)));
  let peel = (n, cur: option(t)) =>
    Option.bind(
      cur,
      fun
      | Point(p) => Option.map(point, Point.peel(n, p))
      | Select(sel) => Option.map(select, Select.peel(n, sel)),
    );
  let union = (l: option(t), r: option(t)) =>
    switch (l, r) {
    | (None, None) => None
    | (Some(_), None)
    | (Some(Select(_)), _) => l
    | (None, Some(_))
    | (_, Some(Select(_))) => r
    | (Some(Point(l)), Some(Point(r))) =>
      let focus = l.is_focus ? l.path : r.path;
      let anchor = l.is_focus ? r.path : l.path;
      Some(Select({focus, anchor}));
    };
  let get_focus = cur =>
    Option.bind(
      cur,
      fun
      | Point(p: Point.t) => p.is_focus ? None : Some(p.path)
      | Select(sel: Select.t) => Some(sel.focus),
    );
  let put_focus = path =>
    fun
    | None
    | Some(Point(_)) => Some(Point(Point.{is_focus: true, path}))
    | Some(Select(sel)) => Some(Select(Select.{...sel, focus: path}));
  let map_paths = f =>
    Option.map(
      Cursor.map(
        (p: Point.t) => {...p, path: f(p.path)},
        ({anchor, focus}: Select.t) =>
          Select.{anchor: f(anchor), focus: f(focus)},
      ),
    );
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
  let map_paths = (f, ghosts) =>
    to_list(ghosts)
    |> List.map(((path, mold)) => (f(path), mold))
    |> of_list;
};

module Marks = {
  [@deriving (sexp, yojson)]
  type t = {
    cursor: option(Cursor.t),
    ghosts: Ghosts.t,
  };
  let mk = (~cursor=?, ~ghosts=Ghosts.empty, ()) => {cursor, ghosts};
  let empty = mk();
  let is_empty = (==)(empty);
  let pp = (out, marks) =>
    if (is_empty(marks)) {
      Fmt.nop(out, marks);
    } else if (Option.is_none(marks.cursor)) {
      Fmt.pf(out, "ghosts: %a", Ghosts.pp, marks.ghosts);
    } else if (Ghosts.is_empty(marks.ghosts)) {
      Fmt.pf(out, "cursor: %a", Cursor.pp, Option.get(marks.cursor));
    } else {
      Fmt.pf(
        out,
        "cursor: %a,@ ghosts: %a",
        Cursor.pp,
        Option.get(marks.cursor),
        Ghosts.pp,
        marks.ghosts,
      );
    };
  let show = Fmt.to_to_string(pp);
  let point = is_focus => mk(~cursor=Point(Point.{is_focus, path: []}), ());
  let put_cursor = (cur, marks) => {...marks, cursor: Some(cur)};
  let get_focus = (marks: t) => Cursor.get_focus(marks.cursor);
  let put_focus = (path: Base.t, marks: t) => {
    ...marks,
    cursor: Cursor.put_focus(path, marks.cursor),
  };
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
  let map_paths = (f, {cursor, ghosts}) => {
    cursor: Cursor.map_paths(f, cursor),
    ghosts: Ghosts.map_paths(f, ghosts),
  };
};
