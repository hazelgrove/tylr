[@deriving sexp]
type t = (Nib.t, Nib.t);

let of_hole = sort => Nib.({sort, shape: Convex}, {sort, shape: Convex});

// let sort_consistent = ((l, r): t, (l', r'): t) =>
//   Nib.sort_consistent(l, l') && Nib.sort_consistent(r, r');

let fitting = ((l, r): t) => (Nib.fitting(l), Nib.fitting(r));

let orient = (d: Util.Direction.t, (l, r)) =>
  switch (d) {
  | Left => (l, r)
  | Right => (r, l)
  };
let unorient = orient;
