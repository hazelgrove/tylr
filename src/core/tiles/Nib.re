module Shape = {
  [@deriving show]
  type t =
    | Convex
    | Concave(Precedence.t);

  let concave = (~p=?, ()) => {
    let p =
      switch (p) {
      | None => Precedence.min
      | Some(p) => p
      };
    Concave(p);
  };

  let fits = (l: t, r: t) =>
    switch (l, r) {
    | (Convex, Concave(_))
    | (Concave(_), Convex) => true
    | (Convex, Convex)
    | (Concave(_), Concave(_)) => false
    };

  let fitting =
    fun
    | Convex => concave()
    | Concave(_) => Convex;

  let flip =
    fun
    | Convex => concave()
    | Concave(_) => Convex;
};

[@deriving show]
type t = {
  shape: Shape.t,
  sort: Sort.t,
};

let fits = (l: t, r: t): bool =>
  l.sort == r.sort && Shape.fits(l.shape, r.shape);

let fitting = (nib: t): t => {...nib, shape: Shape.fitting(nib.shape)};

// let toggle = (nib: t) => {
//   ...nib,
//   orientation: Direction.toggle(nib.orientation),
// };

// let sort_consistent = (nib: t, nib': t) => nib.sort == nib'.sort;

// let of_sort = sort => [
//   {sort, orientation: Left},
//   {sort, orientation: Right},
// ];
