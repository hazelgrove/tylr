open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Util;

module Token = {
  module Point = {
    include Point;
    [@deriving (show({with_path: false}), sexp, yojson, hash)]
    type t = Point.t(Step.t);
    let shift = n => map((+)(n));
  };
  module Selection = {
    include Selection;
    [@deriving (show({with_path: false}), sexp, yojson, hash)]
    type t = Selection.t(Step.Range.t);
    let shift = n => map(Step.Range.map((+)(n)));
  };
  module Cursor = {
    include Cursor;
    [@deriving (show({with_path: false}), sexp, yojson, hash)]
    type t = Cursor.t(Point.t, Selection.t);
  };

  [@deriving (show({with_path: false}), sexp, yojson, hash)]
  type t = option(Cursor.t);

  let add = (p: Point.t, marks: t): t =>
    switch (marks) {
    | None => Some(Point(p))
    | Some(Point(q)) =>
      let (l, r) = Step.compare(p.path, q.path) <= 0 ? (p, q) : (q, p);
      Some(
        Select({focus: l.caret == Focus ? L : R, range: (l.path, r.path)}),
      );
    | Some(Select(_)) => failwith("todo: Marks.Token.add")
    };

  let map: (_, t) => t = Option.map;
  let shift = n => map(Cursor.map(Point.shift(n), Selection.shift(n)));

  let union = (l: t, r: t) =>
    switch (l, r) {
    | (None, None) => None
    | (Some(_), None) => l
    | (None, Some(_)) => r
    | (Some(Select(_)), _) => l
    | (_, Some(Select(_))) => r
    | (Some(Point({path: l, caret})), Some(Point({path: r, _}))) =>
      Some(Select({focus: caret == Focus ? L : R, range: (l, r)}))
    };
};

module Cell = {
  module Point = {
    include Point;
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Point.t(Path.t);
    let mk = (~path=Path.empty, caret) => {path, caret};
    let cons = n => Point.map(Path.cons(n));
    let peel = (n, p: t) =>
      Path.peel(n, p.path) |> Option.map(path => {...p, path});
    let hd = (p: t) =>
      get(Path.hd, p) |> Path.Head.map(Fun.const(p.caret));
  };

  module Selection = {
    include Selection;
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Selection.t(Path.Range.t);
    // note: this check is conservative for non-normalized paths
    let is_empty = get(Path.Range.is_empty);
    let cons = n => map(Path.Range.cons(n));
    let peel = (n, sel: t) =>
      Path.Range.peel(n, sel.range) |> Option.map(range => {...sel, range});
    let hd = get(Path.Range.hd);
    let get_focus = (sel: t) => Dir.pick(sel.focus, sel.range);
    let put_focus = (foc, sel: t) => {
      let (_foc, anc) = Dir.order(sel.focus, sel.range);
      {...sel, range: Dir.order(sel.focus, (foc, anc))};
    };
  };

  module Cursor = {
    module Head = {
      include Path.Head;
      type t = Path.Head.t(Cursor.t(Caret.t, Path.Range.t));
    };

    include Cursor;
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Cursor.t(Point.t, Selection.t);

    let hd: option(t) => option(Head.t) =
      Option.map(
        fun
        | Point(p) => Head.map(Cursor.point, Point.hd(p))
        | Select(sel) => Head.map(Cursor.select, Selection.hd(sel)),
      );
    let cons = n => Option.map(map(Point.cons(n), Selection.cons(n)));
    let peel = (n, cur: option(t)) =>
      Option.bind(
        cur,
        fun
        | Point(p) => Option.map(point, Point.peel(n, p))
        | Select(sel) => Option.map(select, Selection.peel(n, sel)),
      );
    let union = (l: option(t), r: option(t)) =>
      switch (l, r) {
      | (None, None) => None
      | (Some(_), None)
      | (Some(Select(_)), _) => l
      | (None, Some(_))
      | (_, Some(Select(_))) => r
      | (Some(Point({path: l, caret})), Some(Point({path: r, _}))) =>
        Some(Select({focus: caret == Focus ? L : R, range: (l, r)}))
      };
    let get_focus = cur =>
      Option.bind(
        cur,
        fun
        | Select(sel) => Some(Selection.get_focus(sel))
        | Point(p: Point.t) =>
          switch (p.caret) {
          | Anchor => None
          | Focus => Some(p.path)
          },
      );
    let put_focus = path =>
      fun
      | None
      | Some(Point(_)) => Some(Point(Point.focus(path)))
      | Some(Select(sel)) => Some(Select(Selection.put_focus(path, sel)));
    let map_paths = f =>
      Option.map(
        Cursor.map(Point.map(f), Selection.map(Path.Range.map(f))),
      );
  };

  module Ghosts = {
    include Path.Map;
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Path.Map.t(Tile.T.t);
    let cons = (n, ghosts) =>
      to_list(ghosts)
      |> List.rev_map(((path, t)) => ([n, ...path], t))
      |> of_list;
    let peel = (n, ghosts) =>
      to_list(ghosts)
      |> List.filter_map(((path, t)) =>
           switch (path) {
           | [m, ...ms] when m == n => Some((ms, t))
           | _ => None
           }
         )
      |> of_list;
    let union = union((_, m, _) => Some(m));
    let map_paths = (f, ghosts) =>
      to_list(ghosts) |> List.map(((path, t)) => (f(path), t)) |> of_list;
  };

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
  let put_cursor = (cur, marks) => {...marks, cursor: Some(cur)};
  let get_focus = (marks: t) => Cursor.get_focus(marks.cursor);
  let put_focus = (path: Path.t, marks: t) => {
    ...marks,
    cursor: Cursor.put_focus(path, marks.cursor),
  };
  let add_ghost = (~path=Path.empty, t: Tile.T.t, marks: t) => {
    ...marks,
    ghosts: Ghosts.add(path, t, marks.ghosts),
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

  let of_token = (marks: Token.t) =>
    mk(
      ~cursor=?
        Option.map(
          Cursor.map(
            Point.map(Path.of_step),
            Selection.map(Path.Range.map(Path.of_step)),
          ),
          marks,
        ),
      (),
    );

  let to_token = (~len: int, marks: t) =>
    marks.cursor
    |> Option.map(
         Cursor.map(
           Point.map(path => Path.Head.get(Fun.const(0), Path.hd(path))),
           Selection.map(((l, r)) =>
             (
               Path.Head.get(Fun.const(0), Path.hd(l)),
               Path.Head.get(Fun.const(len), Path.hd(r)),
             )
           ),
         ),
       );
};
