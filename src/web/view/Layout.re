type annot =
  | Tile(Decoration.Tile.shape, Decoration.Tile.style)
  | Tessera(Decoration.Tessera.shape, Decoration.Tessera.style)
  | OpenChild
  | ErrHole
  | Delim
  | Selection;

type t =
  | Text(string)
  | Cat(t, t)
  | Annot(annot, t);

let empty = Text("");
let space = Text(Unicode.nbsp);

let sep = (l1, l2) => Cat(Cat(l1, space), l2);
let seps =
  fun
  | [] => empty
  | [l, ...ls] => List.fold_left(sep, l, ls);

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

let mk_Paren = body => seps([delim("("), body, delim(")")]);
let mk_Lam = p => seps([delim(Unicode.lam), p, delim(".")]);
let mk_Let = (p, def) =>
  seps([delim("let"), p, delim("="), def, delim("in")]);
let mk_Ann = ann => seps([delim(":"), ann, delim("")]);
let mk_Plus = () => delim("+");
let mk_Arrow = () => delim(Unicode.right_arrow);
let mk_OpHole = () => Text(Unicode.nbsp);
let mk_BinHole = () => Text(Unicode.nbsp);
