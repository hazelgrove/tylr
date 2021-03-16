open Util;
open New;

type t =
  | Text(string)
  | Cat(t, t)
  | Annot(annot, t)
and annot =
  | Delim
  | UniChild(Sort.t, Direction.t)
  | OpenChild
  | ClosedChild
  | Tessera(Decoration.Tessera.shape, Decoration.Tessera.style)
  | Tile(Decoration.Tile.shape, Decoration.Tile.style)
  | Grout(option(caret))
  | EmptyHole
  | ErrHole(bool)
  | Selection(Decoration.Selection.style)
and caret =
  // TODO generalize to other sorts
  | Pointing(TypeInfo_exp.t)
  | Selecting
  | Restructuring(ZZList.t(t, t));

let empty = Text("");
let space = Text(Unicode.nbsp);

let annot = (ann, l) => Annot(ann, l);

let delim = s => Annot(Delim, Text(s));
let uni_child = (~sort, ~side) => annot(UniChild(sort, side));
let open_child = annot(OpenChild);
let closed_child = annot(ClosedChild);

let root_tile = (~shape, ~sort) =>
  annot(
    Tile(
      shape,
      Decoration.Tile.mk_style(
        ~highlighted=true,
        ~show_children=true,
        ~raised=true,
        ~sort,
        (),
      ),
    ),
  );

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

// let err_hole = (has_err: bool, l: t) => has_err ? Annot(ErrHole, l) : l;

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

let place_caret_before = (_, _) => failwith("todo");
let place_caret_after = (_, _) => failwith("todo");

type with_dangling_caret = (t, option(Direction.t));

let place_caret_0 =
  Option.map(
    fun
    | CaretPosition.Before(_) => Direction.Left
    | After => Right,
  );
let place_caret_1 = (has_caret, child1) =>
  switch (has_caret) {
  | None => (child1, None)
  | Some((_, CaretPosition.Before(0))) => (child1, Some(Direction.Left))
  | Some((caret, Before(_one))) => (place_caret_after(caret, child1), None)
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
      place_caret_after(caret, child1),
      child2,
      None,
    )
  | Some((caret, Before(_two))) => (
      child1,
      place_caret_after(caret, child2),
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

let mk_text = (~has_caret=?, s, ()) => (Text(s), place_caret_0(has_caret));
