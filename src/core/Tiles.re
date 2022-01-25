open Tile;

[@deriving sexp]
type t = s;

let empty = ([], []);
let rev = _ => failwith("todo");
let concat = _ => failwith("todo");

let mk: list(Tile.t) => t = _ => failwith("todo Tiles.mk");

// let glue = ((l, r) as nibs: Nibs.t) =>
//   if (l == r) {
//     [];
//   } else if (l.sort == r.sort) {
//     switch (l.orientation) {
//     | Left => [Hole(l.sort)]
//     | Right => [Sep(nibs)]
//     };
//   } else {
//     let l =
//       switch (l.orientation) {
//       | Left => [Hole(l.sort)]
//       | Right => []
//       };
//     let r =
//       switch (r.orientation) {
//       | Right => [Hole(r.sort)]
//       | Left => []
//       };
//     concat([l, [Sep], r]);
//   };

module Frame = {
  type nonrec t = (t, t);
};
