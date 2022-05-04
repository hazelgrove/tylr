type t =
  | Convex
  // TODO add precedence
  | Concave;

let mk_fits = (s: Nib.Shape.t) =>
  switch (s) {
  | Convex => Concave
  | Concave(_) => Convex
  };

let fits = (g: t, s: Nib.Shape.t): bool =>
  switch (g, s) {
  | (Convex, Convex)
  | (Concave, Concave(_)) => false
  | (Convex, Concave(_))
  | (Concave, Convex) => true
  };
