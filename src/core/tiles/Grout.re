[@deriving show]
type t = {
  sort: Sort.t,
  nibs: Nibs.t,
};

let mk = (nibs, sort) => {nibs, sort};

let mk_opt = ((l, r) as nibs, sort) =>
  Nib.fits(l, r) ? None : Some(mk(nibs, sort));
