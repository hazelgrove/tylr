[@deriving sexp]
type t = (Nib.t, Nib.t);

let of_sort = sort =>
  Nib.({sort, orientation: Left}, {sort, orientation: Right});

let sort_consistent = ((l, r): t, (l', r'): t) =>
  Nib.sort_consistent(l, l') && Nib.sort_consistent(r, r');
