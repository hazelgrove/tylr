open Util;

[@deriving show]
type t =
  | Convex
  | Concave;

let shapes =
  fun
  | Convex => Nib.Shape.(Convex, Convex)
  | Concave => Nib.Shape.(Concave(Precedence.min), Concave(Precedence.min));

// assumes same shape on both sides
let mk_fits_shape = (s: Nib.Shape.t) =>
  switch (s) {
  | Convex => Concave
  | Concave(_) => Convex
  };
let mk_fits = ((l, r): (Nib.Shape.t, Nib.Shape.t)) =>
  Nib.Shape.fits(l, r) ? None : Some(mk_fits_shape(l));

let fits_shape = (g: t, s: Nib.Shape.t) =>
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

let merge = (gs: list(t)): option(t) =>
  switch (gs) {
  | [] => None
  | [hd, ...tl] =>
    ListUtil.split_last_opt(tl)
    |> OptUtil.and_then(((_, ft)) => hd == ft ? Some(hd) : None)
  };
