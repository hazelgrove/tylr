open Util;

[@deriving sexp]
type t = {
  orientation: Direction.t,
  sort: Sort.t,
};

let toggle = (nib: t) => {
  ...nib,
  orientation: Direction.toggle(orientation),
};
