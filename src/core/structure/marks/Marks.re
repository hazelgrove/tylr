open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

exception Invalid;

module Token = {
  [@deriving (show({with_path: false}), sexp, yojson, hash)]
  type t = option(Step.Cursor.t);
  let add = (p: Step.Caret.t, marks: t): t =>
    switch (marks) {
    | None => Some(Point(p))
    | Some(Point(q)) =>
      let (l, r) = Step.compare(p.path, q.path) <= 0 ? (p, q) : (q, p);
      let focus = Dir.(l.hand == Focus ? L : R);
      Some(Select({focus, range: (l.path, r.path)}));
    | Some(Select({focus, range})) =>
      let (foc, anc) = Dir.order(focus, range);
      let (foc, anc) = p.hand == Focus ? (p.path, anc) : (foc, p.path);
      Some(Select({focus, range: Dir.order(focus, (foc, anc))}));
    };
  let shift = n => Option.map(Step.Cursor.map(Step.shift(n)));
  let union = Options.merge(~f=Step.Cursor.union);
};

module Cell = {
  [@deriving (sexp, yojson)]
  type t = {
    cursor: option(Path.Cursor.t),
    ghosts: Path.Map.t(Tile.T.t),
  };
  let mk = (~cursor=?, ~ghosts=Path.Map.empty, ()) => {cursor, ghosts};
  let empty = mk();
  let is_empty = (==)(empty);
  let pp = (out, marks) =>
    if (is_empty(marks)) {
      Fmt.nop(out, marks);
    } else if (Option.is_none(marks.cursor)) {
      Fmt.pf(out, "ghosts: %a", Path.Map.pp(Tile.T.pp), marks.ghosts);
    } else if (Path.Map.is_empty(marks.ghosts)) {
      Fmt.pf(out, "cursor: %a", Path.Cursor.pp, Option.get(marks.cursor));
    } else {
      Fmt.pf(
        out,
        "cursor: %a,@ ghosts: %a",
        Path.Cursor.pp,
        Option.get(marks.cursor),
        Path.Map.pp(Tile.T.pp),
        marks.ghosts,
      );
    };
  let show = Fmt.to_to_string(pp);
  let put_cursor = (cur, marks) => {...marks, cursor: Some(cur)};
  let get_focus = (marks: t) =>
    Option.bind(marks.cursor, Path.Cursor.get_focus);
  let put_focus = (path: Path.t, marks: t) => {
    ...marks,
    cursor: Path.Cursor.put_focus(path, marks.cursor),
  };
  let add_ghost = (~path=Path.empty, t: Tile.T.t, marks: t) => {
    ...marks,
    ghosts: Path.Map.add(path, t, marks.ghosts),
  };
  let map = (f_cursor, f_ghosts, {cursor, ghosts}) => {
    cursor: f_cursor(cursor),
    ghosts: f_ghosts(ghosts),
  };
  let cons = n => map(Option.map(Path.Cursor.cons(n)), Path.Map.cons(n));
  let peel = n =>
    map(Options.bind(~f=Path.Cursor.peel(n)), Path.Map.peel(n));
  let map_paths = f =>
    map(Option.map(Path.Cursor.map_paths(f)), Path.Map.map_paths(f));
  let union = (l: t, r: t) => {
    cursor: Options.merge(~f=Path.Cursor.union, l.cursor, r.cursor),
    ghosts: Path.Map.union((_, t, _) => Some(t), l.ghosts, r.ghosts),
  };
  let union_all = List.fold_left(union, empty);

  let of_token = (marks: Token.t) =>
    mk(~cursor=?Option.map(Path.Cursor.of_step, marks), ());
  let to_token = (~len: int, marks: t): Token.t =>
    Option.map(Path.Cursor.to_step(~len), marks.cursor);
};
