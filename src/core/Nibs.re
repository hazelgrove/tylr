[@deriving sexp]
type t = (Nib.t, Nib.t);

let of_sort = _ => failwith("todo Nibs.of_sort");

let sort_consistent = ((l, r): t, (l', r'): t) =>
  Nib.sort_consistent(l, l') && Nib.sort_consistent(r, r');
