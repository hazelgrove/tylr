open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

module Head = {
  type t('err) = Result.t(Step.t, 'err);
  let map_err = f => Result.map_error(~f);
  let get = f =>
    fun
    | Ok(step) => step
    | Error(err) => f(err);
};

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, hash)]
  type t = list(Step.t);
  let compare = List.compare(Step.compare);
  let lt = (l, r) => compare(l, r) < 0;
  let gt = (l, r) => compare(l, r) > 0;
  let empty = [];
  let of_step = n => [n];
  let cons = List.cons;
  let peel = n =>
    fun
    | [hd, ...tl] when n == hd => Some(tl)
    | _ => None;
  let hd: t => Head.t(unit) =
    fun
    | [hd, ..._] => Ok(hd)
    | [] => Error();
};
include Base;

module Map = {
  include Maps.Make(Base);
  let to_list = bindings;
  let of_list = bindings => of_seq(List.to_seq(bindings));
  let map_paths = (f, map) =>
    to_list(map) |> List.map(Tuples.map_fst(f)) |> of_list;
  let cons = n => map_paths(Base.cons(n));
  let peel = (n, map) =>
    to_list(map)
    |> List.filter_map(((path, t)) =>
         switch (path) {
         | [m, ...ms] when m == n => Some((ms, t))
         | _ => None
         }
       )
    |> of_list;
};

module Range = {
  [@deriving (show({with_path: false}), sexp, yojson, hash)]
  type t = (Base.t, Base.t);
  let mk = (p1: Base.t, p2: Base.t) =>
    Base.compare(p1, p2) <= 0 ? (p1, p2) : (p2, p1);
  let is_empty = ((l, r): t) => Base.compare(l, r) == 0;
  let map = Tuples.map2;
  let hd =
    fun
    | ([hd_l, ..._], [hd_r, ..._]) when hd_l == hd_r => Ok(hd_l)
    | range => Error(range);
  let peel = (n, (l, r): t) => {
    open Options.Syntax;
    let+ l = Base.peel(n, l)
    and+ r = Base.peel(n, r);
    (l, r);
  };
};

module Caret = {
  include Caret;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Caret.t(Base.t);
  let mk = (~path=Base.empty, hand: Caret.Hand.t) => mk(hand, path);
  let cons = n => map(Base.cons(n));
  let peel = (n: Step.t, car: t) =>
    Base.peel(n, car.path) |> Option.map(path => {...car, path});
  let hd = (car: t) =>
    Head.map_err(Fun.const(Caret.mk(car.hand, ())), get(Base.hd, car));
};

module Selection = {
  include Selection;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Selection.t(Range.t);
  // note: this check is conservative for non-normalized paths
  let is_empty = get(Range.is_empty);
  let map2 = f => Selection.map(Range.map(f));
  let cons = n => map2(Base.cons(n));
  let peel = (n, sel: t) =>
    Range.peel(n, sel.range) |> Option.map(range => {...sel, range});
  let hd = sel => Range.hd(sel.range) |> Head.map_err(mk(~focus=sel.focus));
  let get_focus = (sel: t) => Dir.pick(sel.focus, sel.range);
  let put_focus = (foc, sel: t) => {
    let (_foc, anc) = Dir.order(sel.focus, sel.range);
    {...sel, range: Dir.order(sel.focus, (foc, anc))};
  };
  let carets: t => (Caret.t, Caret.t) =
    Selection.carets(~split_range=Fun.id);
  let of_carets = (c1: Caret.t, c2: Caret.t) =>
    Base.compare(c1.path, c2.path) <= 0
      ? mk(~focus=c1.hand == Focus ? L : R, (c1.path, c2.path))
      : mk(~focus=c2.hand == Focus ? L : R, (c2.path, c1.path));
};

module Cursor = {
  include Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Cursor.t(Caret.t, Selection.t);

  let hd =
    fun
    | Point(p) => Head.map_err(Cursor.point, Caret.hd(p))
    | Select(sel) => Head.map_err(Cursor.select, Selection.hd(sel));
  let cons = n => Cursor.map(Caret.cons(n), Selection.cons(n));
  let peel = n =>
    fun
    | Point(p) => Option.map(point, Caret.peel(n, p))
    | Select(sel) => Option.map(select, Selection.peel(n, sel));
  let union = (l: t, r: t) =>
    switch (l, r) {
    | (Select(_), _) => l
    | (_, Select(_)) => r
    | (Point({path: l, hand}), Point({path: r, _})) =>
      Select({focus: hand == Focus ? L : R, range: (l, r)})
    };
  let get_focus =
    fun
    | Select(sel) => Some(Selection.get_focus(sel))
    | Point(car: Caret.t) =>
      switch (car.hand) {
      | Anchor => None
      | Focus => Some(car.path)
      };
  let put_focus = path =>
    fun
    | None
    | Some(Point(_)) => Some(Point(Caret.focus(path)))
    | Some(Select(sel)) => Some(Select(Selection.put_focus(path, sel)));
  let map_paths = f => Cursor.map(Caret.map(f), Selection.map2(f));
  let of_step: Step.Cursor.t => t =
    Cursor.map(Caret.map(Base.of_step), Selection.map2(Base.of_step));
  let to_step = (~len) =>
    Cursor.map(
      Caret.map(path => Head.get(Fun.const(0), Base.hd(path))),
      Selection.map(((l, r)) =>
        (
          Head.get(Fun.const(0), Base.hd(l)),
          Head.get(Fun.const(len), Base.hd(r)),
        )
      ),
    );

  let mk = (c1: Caret.t, c2: Caret.t) =>
    Base.compare(c1.path, c2.path) == 0
      ? Point(Caret.focus(c1.path)) : Select(Selection.of_carets(c1, c2));
};
