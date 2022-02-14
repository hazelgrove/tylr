open Util;

module Affix = {
  [@deriving sexp]
  type t = Tiles.t;

  let empty = Tiles.empty;

  // type nonrec hd = hd;
  // type nonrec tl = tl;
  // let near_nib_tl = (d: Direction.t, tl: tl, far_nib: Nib.t) =>
  //   switch (tl) {
  //   | [] => far_nib
  //   | [(tile, _), ..._] =>
  //     Direction.(choose(toggle(d), Tile.nibs(tile.mold)))
  //   };
  // let near_nib = (d: Direction.t, (hd, tl): t, far_nib: Nib.t) => (
  //   hd,
  //   near_nib_tl(d, tl, far_nib),
  // );
  // let split_hd = (d, affix) =>
  //   switch (affix) {
  //   | [] => None
  //   | [hd, ...tl] =>
  //     let Tile.disassemble(hd))
  //     switch (Tile.disassemble_hd(hd)) {
  //     | [] => Some((hd, tl))
  //     |  =>
  //     }
  //   }
  // let split_hd = split_hd;
  // let reshape_tl = (d: Direction.t, tl: tl, far_nib: Nib.t): list(tl) => {
  //   let fold = ((tile, ps), (tl: tl, k: unit => list(tl))) => {
  //     let tl = [(tile, ps), ...tl];
  //     let k = () =>
  //       switch (Tile.reshape(tile)) {
  //       | [_] => [tl] // short-circuit reshaping when only one option
  //       | reshapings =>
  //         open ListUtil.Syntax;
  //         let* reshaped_tile = reshapings;
  //         let+ reshaped_tl = k();
  //         let adjusted_ps = {
  //           let far_nib = near_nib_tl(d, reshaped_tl, far_nib);
  //           let near_nib = Direction.choose(d, Tile.nibs(tile.mold));
  //           adjust_placeholders(d, ps, (near_nib, far_nib));
  //         };
  //         [(reshaped_tile, adjusted_ps), ...reshaped_tl];
  //       };
  //     (tl, k);
  //   };
  //   let (_, k) = List.fold_right(fold, tl, ([], () => [[]]));
  //   k();
  // };
  // let reshape = (d: Direction.t, (hd, tl): t, far_nib: Nib.t): list(t) =>
  //   reshape_tl(d, tl, far_nib) |> List.map(tl => (hd, tl));

  let split_hd = _ => failwith("todo split_hd");

  let near_nib = (_: Direction.t, _, _) => failwith("near_nib todo");
  let reshape = (_: Direction.t, _, _) => failwith("reshape todo");
};

[@deriving sexp]
type t = (Affix.t, Affix.t);

let empty = (Affix.empty, Affix.empty);

// let near_nibs =
//     ((prefix, suffix): t, far_nibs: Nibs.t): (Grouts.Frame.t, Nibs.t) => {
//   let (grouts_l, near_l) = Affix.near_nib(Left, prefix, fst(far_nibs));
//   let (grouts_r, near_r) = Affix.near_nib(Right, suffix, snd(far_nibs));
//   ((grouts_l, grouts_r), (near_l, near_r));
// };

let cons = (_, _, _) => failwith("todo cons");
let prepend = (_, _, _) => failwith("todo prepend");
let split_piece = (_, _) => failwith("todo splti_piece");
let split_tile = (_, _) => failwith("todo split_tile");

let concat = _ => failwith("todo concat");

let reshape = ((prefix, suffix): t, (far_l, far_r): Nibs.t) => {
  open ListUtil.Syntax;
  let* prefix = Affix.reshape(Left, prefix, far_l);
  let+ suffix = Affix.reshape(Right, suffix, far_r);
  (prefix, suffix);
};

let orient = (d: Direction.t, (prefix, suffix): t): t =>
  switch (d) {
  | Left => (prefix, suffix)
  | Right => (suffix, prefix)
  };
let unorient = orient;

// let split_end = (d: Direction.t, tile: Tile.t): (Tile.t, t) => {
//   let (shard, rest) = Aba.split_end(d, tile.substance);
//   open OptUtil.Syntax;
//   let (front, back) = orient(d, affixes);
//   let+ (piece, front) = Affix.split_hd(front);
//   (piece, unorient(d, (front, back)));
// };

let split_hd = (d: Direction.t, affixes: t): option((Tile.t, t)) => {
  open OptUtil.Syntax;
  let (front, back) = orient(d, affixes);
  let+ (piece, front) = Affix.split_hd(front);
  (piece, unorient(d, (front, back)));
};

let sort: t => Sort.t = _ => failwith("todo Siblings.sort");

let contains_matching = (_, _) => failwith("todo contains_matching");
