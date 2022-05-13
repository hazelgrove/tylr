open Nib.Shape;

[@deriving show]
type t = (Nib.Shape.t, Nib.Shape.t);

let shapes = Fun.id;

// assumes same shape on both sides
let mk_fits_shape = (s: Nib.Shape.t) =>
  switch (s) {
  | Convex => (concave(), concave())
  | Concave(_) => (Convex, Convex)
  };

// assumes same shape on both sides
// currently used such s may be left or right side of g
let fits_shape = (g: t, s: Nib.Shape.t): bool =>
  switch (g, s) {
  | ((_, Convex), Convex)
  | ((_, Concave(_)), Concave(_)) => false
  | ((_, Convex), Concave(_))
  | ((_, Concave(_)), Convex) => true
  };

// assumes same shape on both sides
let fits = (g: t, g': t) =>
  switch (g, g') {
  | ((_, Convex), (Convex, _))
  | ((_, Concave(_)), (Concave(_), _)) => false
  | ((_, Convex), (Concave(_), _))
  | ((_, Concave(_)), (Convex, _)) => true
  };
