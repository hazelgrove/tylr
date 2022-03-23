open Util;

module Prefix = Affix.Make(Orientation.L);
module Suffix = Affix.Make(Orientation.R);

[@deriving show]
type t = (Prefix.t, Suffix.t);

let empty = (Prefix.empty, Suffix.empty);

// let near_nibs =
//     ((prefix, suffix): t, far_nibs: Nibs.t): (Grouts.Frame.t, Nibs.t) => {
//   let (grouts_l, near_l) = Affix.near_nib(Left, prefix, fst(far_nibs));
//   let (grouts_r, near_r) = Affix.near_nib(Right, suffix, snd(far_nibs));
//   ((grouts_l, grouts_r), (near_l, near_r));
// };

let push_tile = (_, _, _) => failwith("todo cons");
let prepend = (_: Direction.t, _, _) => failwith("todo prepend");

let concat = _ => failwith("todo concat");

let of_grouts = _ => failwith("todo of_grouts");

let reshape = ((prefix, suffix): t, (far_l, far_r): Nibs.t) => {
  open ListUtil.Syntax;
  let* prefix = Prefix.reshape(prefix, far_l);
  let+ suffix = Suffix.reshape(suffix, far_r);
  (prefix, suffix);
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

// let connect = (~insert as _=empty, _siblings: t, _s: Sort.t) =>
//   failwith("todo connect");

// let split_grouts = ((prefix, suffix): t): (Grouts.Frame.t, t) => {
//   let (gs_pre, prefix) = Affix.split_nearest_grouts(Left, prefix);
//   let (gs_suf, suffix) = Affix.split_nearest_grouts(Right, suffix);
//   ((gs_pre, gs_suf), (prefix, suffix));
// };

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

let push = (onto: Direction.t, p: Piece.t, (pre, suf): t): t =>
  switch (onto) {
  | Left => (Prefix.push(p, pre), suf)
  | Right => (suf, Suffix.push(p, suf))
  };

let pop =
    (~balanced: bool, from: Direction.t, (pre, suf): t)
    : option((Piece.t, t)) =>
  OptUtil.Syntax.(
    switch (from) {
    | Left =>
      let+ (p, pre) = Prefix.pop(~balanced, pre);
      (p, (pre, suf));
    | Right =>
      let+ (p, suf) = Suffix.pop(~balanced, suf);
      (p, (pre, suf));
    }
  );
