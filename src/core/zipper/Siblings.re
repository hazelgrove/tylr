open Util;

module Affix = {
  [@deriving sexp]
  type t = Tiles.t;

  let empty = Tiles.empty;

  let split_nearest_grouts = (_: Direction.t, _: t) =>
    failwith("todo split_nearest_grouts");

  // let sort_stacks = (d: Direction.t, affix: t): (Sort.Stack.t, Sort.Stack.t) => {
  //   let (pushed, popped) =
  //     Tiles.fold_right(
  //       (tile: Tile.t, (pushed, popped)) =>
  //         switch (tile) {
  //         | Intact(intact) => stack
  //         | Pieces(pieces) =>
  //           open Sort.Stack;
  //           let s = Pieces.sort(ps);
  //           let (front, back) =
  //             Nibs.orient(Direction.toggle(d), Pieces.nibs(ps));
  //           if (s == front.sort && s == back.sort) {
  //             stacks;
  //           } else if (s == front.sort) {
  //             (push(s, pushed), popped);
  //           } else if (s == back.sort) {
  //             switch (pop(pushed)) {
  //             | None => (pushed, push(s, popped))
  //             | Some((_, pushed)) => (pushed, popped)
  //             };
  //           } else {
  //             switch (pop(pushed)) {
  //             | None => (push(s, pushed), push(s, popped))
  //             | Some((_, pushed)) => (push(s, pushed), popped)
  //             };
  //           };
  //         },
  //       affix,
  //       Sort.Stack.(empty, empty),
  //     );
  //   (pushed, Sort.Stack.rev(popped));
  // };
  // let sort_stack = (d, affix) => fst(sort_stacks(d, affix));

  // let split_nearest_grouts = (d: Direction.t, affix: t) =>
  //   switch (affix) {
  //   | []
  //   | [Intact(_), ..._] => ([], affix)
  //   | [Pieces(pieces), ...affix'] =>
  //     let pieces = d == Left ? Aba.rev(pieces) : pieces;
  //     switch (pieces) {
  //     | (Shard(_), _) => ([], affix)
  //     | (Grout(g), []) =>
  //       let (gs, affix) = split_nearest_grouts(affix');
  //       ([g, ...gs], affix);
  //     | (Grout(g), [_, ..._] as tl) =>
  //       let affix'' =
  //         tl
  //         |> List.map(((tiles, piece)) =>

  //         )

  //       ([g], )

  //       let gs =
  //         switch (tl) {
  //         |
  //         }
  //     }

  //   | [Pieces((Shard(_), _)), ..._]
  //   | [Pieces((Grout(g), tl)), ...affix] =>
  //     let gs =
  //       switch (tl) {
  //       | [] => split_nearest_grouts(d)
  //       }
  //   }

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

let push_tile = (_, _, _) => failwith("todo cons");
let prepend = (_: Direction.t, _, _) => failwith("todo prepend");
let pop = (_, _) => failwith("todo pop");
let pop_tile = (_, _): option((Tile.t, t)) => failwith("todo pop_tile");

let concat = _ => failwith("todo concat");

let of_grouts = _ => failwith("todo of_grouts");

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

// let connect = (~insert=empty, affixes: t, s: Sort.t): (t, Frame.t) => {
//   let ctx = failwith("todo ctx");
//   let ((tiles_l, prefix_tl), (tiles_r, suffix_tl)) = affixes;
//   let nibs = Frame.near_nibs_tls((prefix_tl, suffix_tl), Nibs.of_sort(s));
//   let (insertion, grouts, tiles) =
//     remold_reshape(insert, (tiles_l, tiles_r), nibs, ctx)
//     |> List.sort(((_, grouts, _), (_, grouts', _)) =>
//          Grouts.Frame.(Int.compare(size(grouts), size(grouts')))
//        )
//     |> ListUtil.hd_opt
//     |> OptUtil.get_or_fail("Segment.connect: expected at least one remolding");
//   (insertion, Frame.(concat([of_grouts(grouts), of_tiles(tiles)])));
// };
let connect = (~insert as _=empty, _siblings: t, _s: Sort.t) =>
  failwith("todo connect");

let split_grouts = ((prefix, suffix): t): (Grouts.Frame.t, t) => {
  let (gs_pre, prefix) = Affix.split_nearest_grouts(Left, prefix);
  let (gs_suf, suffix) = Affix.split_nearest_grouts(Right, suffix);
  ((gs_pre, gs_suf), (prefix, suffix));
};

// regrout around subject
// let regrout = (siblings: t, s: Sort.t) => {
//   let (gs, siblings') = split_nearest_grouts(siblings);
//   let gs' = Grouts.regrout(Grouts.Frame.to_list(gs), s);
//   Grouts.Frame.to_list(gs) == gs'
//     ? siblings : prepend(Right, Tiles.of_grouts(gs'), siblings');
// };

// let sort_stacks = ((prefix, suffix): t): (Sort.Stack.t, Sort.Stack.t) => (
//   Affix.sort_stack(Left, prefix),
//   Affix.sort_stack(Right, suffix),
// );
