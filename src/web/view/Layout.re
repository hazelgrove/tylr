open Sexplib.Std;
open Util;
open Core;

[@deriving sexp]
type tile_shape = Tile.t(bool, unit, unit, bool);
[@deriving sexp]
type tile_style = {
  sort: option(Sort.t),
  highlighted: bool,
  show_children: bool,
  raised: bool,
  stretched: bool,
};
let mk_tile_style =
    (
      ~highlighted=false,
      ~show_children=false,
      ~raised=false,
      ~stretched=false,
      ~sort=?,
      (),
    ) => {
  sort,
  highlighted,
  show_children,
  raised,
  stretched,
};

[@deriving sexp]
type tessera_shape = Tile.t(unit, bool, bool, (bool, bool));
[@deriving sexp]
type tessera_style = {
  highlighted: bool,
  stretched: bool,
  raised: bool,
};
let mk_tessera_style =
    (~stretched=false, ~raised=false, ~highlighted=false, ()) => {
  stretched,
  raised,
  highlighted,
};

[@deriving sexp]
type selection_style = {unfocused: bool};

[@deriving sexp]
type t =
  | Text(string)
  | Cat(t, t)
  | Annot(annot, t)
and annot =
  | Delim
  | UniChild(Sort.t, Direction.t)
  | OpenChild
  | ClosedChild
  | Tessera(tessera_shape, tessera_style)
  | Tile(tile_shape, tile_style)
  | Grout(option(caret))
  | EmptyHole(option(Sort.t))
  | ErrHole(bool)
  | Selection(selection_style)
and caret =
  | Pointing(CursorInfo.t)
  | Selecting
  | Restructuring(t, ListUtil.frame(t));

type frame = t => t;

let empty = Text("");
let space = Text(Unicode.nbsp);

let annot = (ann, l) => Annot(ann, l);

let delim = s => Annot(Delim, Text(s));
let uni_child = (~sort, ~side) => annot(UniChild(sort, side));
let open_child = annot(OpenChild);
let closed_child = annot(ClosedChild);

let empty_hole = (~sort=?) => annot(EmptyHole(sort));

let root_tile = (~has_caret, ~shape, ~sort) =>
  switch (has_caret) {
  | None => (l => l)
  | Some(_) =>
    annot(
      Tile(
        shape,
        mk_tile_style(
          ~highlighted=true,
          ~show_children=true,
          ~raised=true,
          ~sort,
          (),
        ),
      ),
    )
  };

let cat = (l1, l2) => Cat(l1, l2);
let cats =
  fun
  | [] => empty
  | [l, ...ls] => List.fold_left(cat, l, ls);

let grout = (~caret=?, ()) => Annot(Grout(caret), space);
let grouts = ls =>
  ls
  |> List.map(l => [l, grout()])
  |> List.flatten
  |> List.cons(grout())
  |> cats;
let grouts_l = ls =>
  ls |> List.map(l => [grout(), l]) |> List.flatten |> cats;
let grouts_r = ls =>
  ls |> List.map(l => [l, grout()]) |> List.flatten |> cats;
let grouts_z = (ls_pre, caret, ls_suf) =>
  cats([grouts_l(ls_pre), grout(~caret, ()), grouts_r(ls_suf)]);

let grouts_inner = ls => cats(ListUtil.join(grout(), ls));

let err_hole = (has_err: bool, expanded: bool, l: t) =>
  has_err ? Annot(ErrHole(expanded), l) : l;

let length = {
  let rec go =
    lazy(
      Memo.memoize(
        fun
        | Text(s) => Unicode.length(s)
        | Cat(l1, l2) => Lazy.force(go, l1) + Lazy.force(go, l2)
        | Annot(_, l) => Lazy.force(go, l),
      )
    );
  Lazy.force(go);
};

type measurement = {
  start: int,
  len: int,
};

let measured_fold' =
    (
      ~text: (measurement, string) => 'acc,
      ~cat: (measurement, 'acc, 'acc) => 'acc,
      // let client cut off recursion
      ~annot: (t => 'acc, measurement, annot, t) => 'acc,
      ~start=0,
      l: t,
    ) => {
  let rec go = (~start, l: t) => {
    let m = {start, len: length(l)};
    switch (l) {
    | Text(s) => text(m, s)
    | Cat(l1, l2) =>
      let mid = start + length(l1);
      cat(m, go(~start, l1), go(~start=mid, l2));
    | Annot(ann, l) => annot(go(~start), m, ann, l)
    };
  };
  go(~start, l);
};

let measured_fold = (~annot: (measurement, annot, 'acc) => 'acc, ~start=0) =>
  measured_fold'(~annot=(k, m, ann, l) => annot(m, ann, k(l)), ~start);

let rec place_caret = (d: Direction.t, caret, l) =>
  switch (l) {
  | Text(_) => l
  | Cat(l1, l2) =>
    switch (d) {
    | Left => Cat(place_caret(d, caret, l1), l2)
    | Right => Cat(l1, place_caret(d, caret, l2))
    }
  | Annot(Grout(_), l) => Annot(Grout(Some(caret)), l)
  | Annot(annot, l) => Annot(annot, place_caret(d, caret, l))
  };

type with_dangling_caret = (t, option(Direction.t));

let place_caret_0: option((caret, CaretPosition.t)) => option(Direction.t) =
  Option.map(
    fun
    | (_, CaretPosition.Before(_)) => Direction.Left
    | (_, After) => Right,
  );
let place_caret_1 = (has_caret, child1) =>
  switch (has_caret) {
  | None => (child1, None)
  | Some((_, CaretPosition.Before(0))) => (child1, Some(Direction.Left))
  | Some((caret, Before(_one))) => (
      place_caret(Right, caret, child1),
      None,
    )
  | Some((_, After)) => (child1, Some(Right))
  };
let place_caret_2 = (has_caret, child1, child2) =>
  switch (has_caret) {
  | None => (child1, child2, None)
  | Some((_, CaretPosition.Before(0))) => (
      child1,
      child2,
      Some(Direction.Left),
    )
  | Some((caret, Before(1))) => (
      place_caret(Right, caret, child1),
      child2,
      None,
    )
  | Some((caret, Before(_two))) => (
      child1,
      place_caret(Right, caret, child2),
      None,
    )
  | Some((_, After)) => (child1, child2, Some(Right))
  };

let mk_Paren = (~has_caret: option((caret, CaretPosition.t))=?, body) => {
  let (body, dangling_caret) = place_caret_1(has_caret, body);
  let l = cats([delim("("), open_child(body), delim(")")]);
  (l, dangling_caret);
};

let mk_Lam = (~has_caret=?, p) => {
  let (p, dangling_caret) = place_caret_1(has_caret, p);
  let l = cats([delim(Unicode.lam), closed_child(p), delim(".")]);
  (l, dangling_caret);
};

let mk_Let = (~has_caret=?, p, def) => {
  let (p, def, dangling_caret) = place_caret_2(has_caret, p, def);
  let l =
    cats([
      delim("let"),
      closed_child(p),
      delim("="),
      open_child(def),
      delim("in"),
    ]);
  (l, dangling_caret);
};

let mk_Cond = (~has_caret=?, then_) => {
  let (then_, dangling_caret) = place_caret_1(has_caret, then_);
  let l = cats([delim("?"), open_child(then_), delim(":")]);
  (l, dangling_caret);
};

let mk_Ann = (~has_caret=?, ann) => {
  let (ann, dangling_caret) = place_caret_1(has_caret, ann);
  let l = cats([delim(":"), closed_child(ann), delim("")]);
  (l, dangling_caret);
};

let mk_Plus = (~has_caret=?, ()) => (delim("+"), place_caret_0(has_caret));

let mk_Arrow = (~has_caret=?, ()) => (
  delim(Unicode.right_arrow),
  place_caret_0(has_caret),
);

let mk_OpHole = (~has_caret=?, ()) => (
  Text(Unicode.nbsp),
  place_caret_0(has_caret),
);

let mk_BinHole = (~has_caret=?, ()) => (
  Text(Unicode.nbsp),
  place_caret_0(has_caret),
);

let mk_text = (~has_caret=?, s) => (Text(s), place_caret_0(has_caret));

let decorate_term =
    (~sort, ~is_op_hole, ~is_bin_hole, ~has_caret, f_op, f_pre, f_post, f_bin) => {
  let uni_child = uni_child(~sort);
  let root_tile = root_tile(~has_caret, ~sort);
  Term.get(
    op => {
      let is_op_hole = is_op_hole(op);
      let (op, dangling_caret) = f_op(op);
      let op =
        root_tile(
          ~shape=Op(is_op_hole),
          is_op_hole ? empty_hole(~sort, op) : op,
        );
      switch (dangling_caret) {
      | None => grouts([op])
      | Some(d) =>
        let caret = fst(Option.get(has_caret));
        d == Direction.Left
          ? grouts_z([], caret, [op]) : grouts_z([op], caret, []);
      };
    },
    ((pre, r)) => {
      let ((pre, dangling_caret), r) = f_pre((pre, r));
      let pre = root_tile(~shape=Pre(), pre);
      let r = Option.is_some(has_caret) ? uni_child(~side=Right, r) : r;
      switch (dangling_caret) {
      | None => cat(grouts_l([pre]), r)
      | Some(side) =>
        let caret = fst(Option.get(has_caret));
        switch ((side: Direction.t)) {
        | Direction.Left => cats([grout(~caret, ()), pre, r])
        | Right => cat(grouts_l([pre]), place_caret(Left, caret, r))
        };
      };
    },
    ((l, post)) => {
      let (l, (post, dangling_caret)) = f_post((l, post));
      let post = root_tile(~shape=Post(), post);
      let l = Option.is_some(has_caret) ? uni_child(~side=Left, l) : l;
      switch (dangling_caret) {
      | None => cat(l, grouts_r([post]))
      | Some(side) =>
        let caret = fst(Option.get(has_caret));
        switch (side) {
        | Direction.Left =>
          cat(place_caret(Right, caret, l), grouts_r([post]))
        | Right => cats([l, post, grout(~caret, ())])
        };
      };
    },
    ((l, bin, r)) => {
      let is_bin_hole = is_bin_hole(bin);
      let (l, (bin, dangling_caret), r) = f_bin((l, bin, r));
      let bin =
        root_tile(
          ~shape=Bin(is_bin_hole),
          is_bin_hole ? empty_hole(~sort, bin) : bin,
        );
      let (l, r) =
        Option.is_some(has_caret)
          ? (uni_child(~side=Left, l), uni_child(~side=Right, r)) : (l, r);
      switch (dangling_caret) {
      | None => cats([l, bin, r])
      | Some(side) =>
        let caret = fst(Option.get(has_caret));
        switch (side) {
        | Direction.Left => cats([place_caret(Right, caret, l), bin, r])
        | Right => cats([l, bin, place_caret(Left, caret, r)])
        };
      };
    },
  );
};
