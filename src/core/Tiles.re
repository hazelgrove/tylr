open Sexplib.Std;
open Tile;

[@deriving sexp]
type t = Tile.s;

let empty = [];
let rev = List.rev;
let concat = List.concat;

let glue = ((l, r): Nibs.t) =>
  if (l == r) {
    []
  } else if (l.sort == r.sort) {
    switch (l.orientation) {
    | Left => [Hole(l.sort)]
    | Right => [Sep]
    }
  } else {
    let l =
      switch (l.orientation) {
      | Left => [Hole(l.sort)]
      | Right => []
      };
    let r =
      switch (r.orientation) {
      | Right => [Hole(r.sort)]
      | Left => []
      };
    concat([l, [Sep], r]);
  };

module Frame = {
  type t = ListFrame.t(Tile.t);
};