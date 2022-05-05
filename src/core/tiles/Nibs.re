[@deriving show]
type t = (Nib.t, Nib.t);

let of_hole = sort => Nib.({sort, shape: Convex}, {sort, shape: Convex});

let fitting = ((l, r): t) => (Nib.fitting(l), Nib.fitting(r));
