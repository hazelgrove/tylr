[@deriving show]
type t =
  | Convex
  // TODO add precedence
  | Concave;

let mk_fits_shape = (s: Nib.Shape.t) =>
  switch (s) {
  | Convex => Concave
  | Concave(_) => Convex
  };

let fits_shape = (g: t, s: Nib.Shape.t): bool =>
  switch (g, s) {
  | (Convex, Convex)
  | (Concave, Concave(_)) => false
  | (Convex, Concave(_))
  | (Concave, Convex) => true
  };

let fits = (g: t, g': t) =>
  switch (g, g') {
  | (Convex, Convex)
  | (Concave, Concave) => false
  | (Convex, Concave)
  | (Concave, Convex) => true
  };
