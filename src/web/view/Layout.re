type annot =
  | Tile
  | Tessera
  | OpenChild
  | Term
  | ErrHole;

type t =
  | Text(string)
  | Annot(annot, t)
  | Cat(t, t);

let empty = Text("");
let space = Text(Unicode.nbsp);

let child = (step, l) => Annot(Step(step), l);

let cat = (l1, l2) => Cat(l1, l2);
let cats =
  fun
  | [] => empty
  | [l, ...ls] => List.fold_left(cat, l, ls);

let sep = (l1, l2) => Cat(Cat(l1, space), l2);
let seps =
  fun
  | [] => empty
  | [l, ...ls] => List.fold_left(sep, l, ls);

let err_hole = (has_err: bool, l: t) => has_err ? Annot(ErrHole, l) : l;
