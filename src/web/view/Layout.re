open Util;
open New;

type t =
  | Text(string)
  | Cat(t, t)
  | Annot(annot, t)
and annot =
  | Tile(Decoration.Tile.shape, Decoration.Tile.style)
  | Tessera(Decoration.Tessera.shape, Decoration.Tessera.style)
  | OpenChild
  | ErrHole
  | Delim
  | Selection(Decoration.Selection.style)
  | Grout(option(caret))
and caret =
  // TODO generalize to other sorts
  | Pointing(TypeInfo_exp.t)
  | Selecting
  | Restructuring(ZZList.t(t, t));

let empty = Text("");
let space = Text(Unicode.nbsp);

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

let err_hole = (has_err: bool, l: t) => has_err ? Annot(ErrHole, l) : l;

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

let delim = s => Annot(Delim, Text(s));

let mk_Paren = body => cats([delim("("), body, delim(")")]);
let mk_Lam = p => cats([delim(Unicode.lam), p, delim(".")]);
let mk_Let = (p, def) =>
  cats([delim("let"), p, delim("="), def, delim("in")]);
let mk_Ann = ann => cats([delim(":"), ann, delim("")]);
let mk_Plus = () => delim("+");
let mk_Arrow = () => delim(Unicode.right_arrow);
let mk_OpHole = () => Text(Unicode.nbsp);
let mk_BinHole = () => Text(Unicode.nbsp);
