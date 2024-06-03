open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Util;

module Head = {
  type t('err) = Result.t(Step.t, 'err);
  let map = f => Result.map_error(~f);
  let get = f =>
    fun
    | Ok(step) => step
    | Error(err) => f(err);
};

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, hash)]
  type t = list(Step.t);
  let compare = List.compare(Step.compare);
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
  let is_empty = ((l, r): t) => Base.compare(l, r) == 0;
  let map = Tuples.map2;
  let hd = (~len) =>
    fun
    | ([hd_l, ..._], [hd_r, ..._]) when hd_l == hd_r => Ok(hd_l)
    | (l, r) =>
      Error((
        Head.get(Fun.const(0), Base.hd(l)),
        Head.get(Fun.const(len), Base.hd(r)),
      ));
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
  let mk = (~path=Base.empty, hand) => mk(hand, path);
  let cons = n => map(Base.cons(n));
  let peel = (n: Step.t, car: t) =>
    Base.peel(n, car.path) |> Option.map(path => {...car, path});
  let hd = (car: t) =>
    Head.map(Fun.const(Caret.mk(car.hand, ())), get(Base.hd, car));
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
  let hd = (~len, sel) =>
    Range.hd(~len, sel.range) |> Head.map(mk(~focus=sel.focus));
  let get_focus = (sel: t) => Dir.pick(sel.focus, sel.range);
  let put_focus = (foc, sel: t) => {
    let (_foc, anc) = Dir.order(sel.focus, sel.range);
    {...sel, range: Dir.order(sel.focus, (foc, anc))};
  };
};

module Cursor = {
  include Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Cursor.t(Caret.t, Selection.t);

  let hd = (~len) =>
    Option.map(
      fun
      | Point(p) => Head.map(Cursor.point, Caret.hd(p))
      | Select(sel) => Head.map(Cursor.select, Selection.hd(~len, sel)),
    );
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
  let get_focus = cur =>
    Option.bind(
      cur,
      fun
      | Select(sel) => Some(Selection.get_focus(sel))
      | Point(car: Caret.t) =>
        switch (car.hand) {
        | Anchor => None
        | Focus => Some(car.path)
        },
    );
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
};
