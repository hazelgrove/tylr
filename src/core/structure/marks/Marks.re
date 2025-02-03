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
    | Some(Point(q)) when q.hand == p.hand => Some(Point(p))
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
  let focus_point: t => t =
    Option.map(
      Cursor.map((car: Step.Caret.t) => Step.Caret.focus(car.path), Fun.id),
    );
};

module Cell = {
  [@deriving (sexp, yojson)]
  type t = {
    cursor: option(Path.Cursor.t),
    // todo: unify this with Oblig module
    obligs: Path.Map.t(Mtrl.T.t),
    // cells for linter to inspect
    dirty: Path.Map.t(unit),
    // cells for grouter to prioritize as regrouting spots
    degrouted: Path.Map.t(unit),
  };
  let mk =
      (
        ~cursor=?,
        ~obligs=Path.Map.empty,
        ~dirty=Path.Map.empty,
        ~degrouted=Path.Map.empty,
        (),
      ) => {
    cursor,
    obligs,
    dirty,
    degrouted,
  };
  let empty = mk();
  let is_empty = (==)(empty);
  let pp = (out, {cursor, obligs, dirty, degrouted} as marks) =>
    if (is_empty(marks)) {
      Fmt.nop(out, marks);
    } else if (Option.is_none(cursor)
               && Path.Map.is_empty(dirty)
               && Path.Map.is_empty(degrouted)) {
      Fmt.pf(out, "obligs: %a", Path.Map.pp(Mtrl.T.pp), obligs);
    } else if (Path.Map.is_empty(obligs)
               && Path.Map.is_empty(dirty)
               && Path.Map.is_empty(degrouted)) {
      Fmt.pf(out, "cursor: %a", Path.Cursor.pp, Option.get(cursor));
    } else {
      Fmt.pf(
        out,
        "cursor: %a,@ obligs: %a,@ dirty: %a,@ degrouted: %a",
        Fmt.option(Path.Cursor.pp),
        cursor,
        Path.Map.pp(Mtrl.T.pp),
        obligs,
        Path.Map.pp(Fmt.sp),
        dirty,
        Path.Map.pp(Fmt.sp),
        degrouted,
      );
    };
  let show = Fmt.to_to_string(pp);

  let put_cursor = (cur, marks) => {...marks, cursor: Some(cur)};
  let get_focus = (marks: t) =>
    Option.bind(marks.cursor, Path.Cursor.get_focus);
  // let map_focus = (marks: t) => Path.Cursor.get_focus(marks.cursor);
  let put_focus = (~save_anchor=false, foc: Path.t, marks: t) => {
    ...marks,
    cursor:
      switch (marks.cursor) {
      | None => Path.Cursor.put_focus(foc, marks.cursor)
      | Some(_) when !save_anchor => Some(Point(Caret.focus(foc)))
      | Some(cur) =>
        let anc =
          switch (cur) {
          | Point(car) => car.path
          | Select(sel) => Path.Selection.get_anchor(sel)
          };
        switch (
          Path.Selection.of_carets(Caret.focus(foc), Caret.anchor(anc))
        ) {
        | Ok(sel) => Some(Path.Cursor.select(sel))
        | Error(car) => Some(Path.Cursor.point(car))
        };
      },
  };

  let add_oblig = (~path=Path.empty, t: Mtrl.T.t, marks: t) => {
    ...marks,
    obligs: Path.Map.add(path, t, marks.obligs),
  };

  let dirty = mk(~dirty=Path.Map.singleton(Path.empty, ()), ());
  let mark_clean = marks => {...marks, dirty: Path.Map.empty};

  // clear all temporary marks used by grouter/linter
  let flush = marks => {
    ...marks,
    dirty: Path.Map.empty,
    degrouted: Path.Map.empty,
  };

  let map =
      (
        ~cursor=Fun.id,
        ~obligs=Fun.id,
        ~dirty=Fun.id,
        ~degrouted=Fun.id,
        marks: t,
      ) => {
    cursor: cursor(marks.cursor),
    obligs: obligs(marks.obligs),
    dirty: dirty(marks.dirty),
    degrouted: degrouted(marks.degrouted),
  };
  let cons = n =>
    map(
      ~cursor=Option.map(Path.Cursor.cons(n)),
      ~obligs=Path.Map.cons(n),
      ~dirty=Path.Map.cons(n),
      ~degrouted=Path.Map.cons(n),
    );
  let peel = n =>
    map(
      ~cursor=Options.bind(~f=Path.Cursor.peel(n)),
      ~obligs=Path.Map.peel(n),
      ~dirty=Path.Map.peel(n),
      ~degrouted=Path.Map.peel(n),
    );
  let map_paths = f =>
    map(
      ~cursor=Option.map(Path.Cursor.map_paths(f)),
      ~obligs=Path.Map.map_paths(f),
      ~dirty=Path.Map.map_paths(f),
      ~degrouted=Path.Map.map_paths(f),
    );
  let union = (l: t, r: t) => {
    cursor: Options.merge(~f=Path.Cursor.union, l.cursor, r.cursor),
    obligs: Path.Map.union((_, t, _) => Some(t), l.obligs, r.obligs),
    dirty: Path.Map.union((_, (), ()) => Some(), l.dirty, r.dirty),
    degrouted:
      Path.Map.union((_, (), ()) => Some(), l.degrouted, r.degrouted),
  };
  let union_all = List.fold_left(union, empty);

  let of_token = (marks: Token.t) =>
    mk(~cursor=?Option.map(Path.Cursor.of_step, marks), ());
  let to_token = (~len: int, marks: t): Token.t =>
    Option.map(Path.Cursor.to_step(~len), marks.cursor);
};
