open Tile;
open Util;

[@deriving sexp]
type t = s;

// type hd = Grouts.t;
// type tl = Util.Baba.t(Tile.t, Grouts.t);

// let cons_grout = (grout, (hd, tl)) => ([grout, ...hd], tl);
// let cons_tile = (tile, (hd, tl)) => ([], [(tile, hd), ...tl]);

let empty = ([], []);
let rev = _ => failwith("todo Tiles.rev");
let concat = _ => failwith("todo Tiles.concat");

// let mk: list(Tile.t) => t = _ => failwith("todo Tiles.mk");

module Affix = {
  [@deriving sexp]
  type nonrec t = t;
  // type nonrec hd = hd;
  // type nonrec tl = tl;

  let near_nib_tl = (d: Direction.t, tl: tl, far_nib: Nib.t) =>
    switch (tl) {
    | [] => far_nib
    | [(tile, _), ..._] =>
      Direction.(choose(toggle(d), Tile.nibs(tile.mold)))
    };
  let near_nib = (d: Direction.t, (hd, tl): t, far_nib: Nib.t) => (
    hd,
    near_nib_tl(d, tl, far_nib),
  );

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

  let reshape_tl = (d: Direction.t, tl: tl, far_nib: Nib.t): list(tl) => {
    let fold = ((tile, ps), (tl: tl, k: unit => list(tl))) => {
      let tl = [(tile, ps), ...tl];
      let k = () =>
        switch (Tile.reshape(tile)) {
        | [_] => [tl] // short-circuit reshaping when only one option
        | reshapings =>
          open ListUtil.Syntax;
          let* reshaped_tile = reshapings;
          let+ reshaped_tl = k();
          let adjusted_ps = {
            let far_nib = near_nib_tl(d, reshaped_tl, far_nib);
            let near_nib = Direction.choose(d, Tile.nibs(tile.mold));
            adjust_placeholders(d, ps, (near_nib, far_nib));
          };
          [(reshaped_tile, adjusted_ps), ...reshaped_tl];
        };
      (tl, k);
    };
    let (_, k) = List.fold_right(fold, tl, ([], () => [[]]));
    k();
  };
  let reshape = (d: Direction.t, (hd, tl): t, far_nib: Nib.t): list(t) =>
    reshape_tl(d, tl, far_nib) |> List.map(tl => (hd, tl));
};

module Frame = {
  [@deriving sexp]
  type t = (Affix.t, Affix.t);

  let near_nibs =
      ((prefix, suffix): t, far_nibs: Nibs.t): (Grouts.Frame.t, Nibs.t) => {
    let (grouts_l, near_l) = Affix.near_nib(Left, prefix, fst(far_nibs));
    let (grouts_r, near_r) = Affix.near_nib(Right, suffix, snd(far_nibs));
    ((grouts_l, grouts_r), (near_l, near_r));
  };

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

  let split_end = (d: Direction.t, tile: Tile.t): (Tile.t, t) => {
    let (shard, rest) = Aba.split_end(d);
    open OptUtil.Syntax;
    let (front, back) = orient(d, affixes);
    let+ (piece, front) = Affix.split_hd(front);
    (piece, unorient(d, (front, back)));
  };

  let split_hd = (d: Direction.t, affixes: t): option((Tile.t, t)) => {
    open OptUtil.Syntax;
    let (front, back) = orient(d, affixes);
    let+ (piece, front) = Affix.split_hd(front);
    (piece, unorient(d, (front, back)));
  };
};

let split_by_placeholder = (tiles: t): Util.Aba.t(t, Shard.Placeholder.t) => {};

let rec split_by_matching_label = (tiles: t): Util.Aba.t(t, Shard.Labeled.t) => {
  let (split, _) =
    List.fold_right(
      (tile, (split_suffix, found_labeled_shard)) =>
        switch (tile) {
        | Labeled(_) =>
          let (hd, tl) = split_suffix;
          (([tile, ...hd], tl), found_labeled_shard);
        | Placeholder(p) =>
          let (split_p, found) =
            split_placeholder_by_matching_label(found_labeled_shard, p);
          (Aba.concat((@), [split_p, split_suffix]), found);
        },
      tiles,
      (([], []), None),
    );
  split;
}
and split_placeholder_by_matching_label =
    (p: t, found_labeled_shard: option(Tile.Id.t))
    : (Util.Aba.t(s, Shard.Labeled.t), option(Tile.Id.t)) => {
  let cons_shard = (shard: Shard.t, ((hd, tl), found)) =>
    switch (shard, found) {
    | (Labeled({tile: (id, _), _} as labeled), None) => (
        ([], [(labeled, hd), ...tl]),
        Some(id),
      )
    | (Labeled({tile: (id, _), _} as labeled), Some(id')) when id == id' => (
        ([], [(labeled, hd), ...tl]),
        found,
      )
    | (shard, _) => (([Tile.Placeholder((shard, [])), ...hd], tl), found)
    };
  p
  |> List.fold_right(
       shard => cons_shard(shard, ([], [])),
       (shard, tiles, (found, split_suffix)) => {
         let split_tiles = split_by_matching_label(tiles);
         cons_shard(shard, Aba.concat((@), [split_tiles, split_suffix]));
       },
     );
};

let rec reassemble = (tiles: t): t => {
  let (hd, tl) = split_by_matching_label(tiles);
  switch (Baba.split_last_opt(tl)) {
  | None => hd
  | Some((tile_split, last)) =>
    let tile = Tile.mk(tile_split);
    reassemble(hd) @ [tile, ...last];
  };
};

/**
 * should never need
 */
// let remold = (tiles: t, nibs: Nibs.t, ctx: Shard.Ctx.t): list(t) => {
//   open ListUtil.Syntax;
//   tiles
//   |> List.fold_left(
//     (((l, r), ctx), tile) =>
//       switch (tile) {
//       | Placeholder((Labeled(shard), [])) =>
//         let* nibs = Shard.assignable_nibs(~l, ctx, shard);
//         let shard = {...shard};
//         let ctx = Shard.Ctx.add(shard, ctx);

//       }
//     (nibs, ctx),
//   )
// };

let connect = (~insert=empty, affixes: Frame.t, s: Sort.t): (t, Frame.t) => {
  let ctx = failwith("todo ctx");
  let ((tiles_l, prefix_tl), (tiles_r, suffix_tl)) = affixes;
  let nibs = Frame.near_nibs_tls((prefix_tl, suffix_tl), Nibs.of_sort(s));
  let (insertion, grouts, tiles) =
    remold_reshape(insert, (tiles_l, tiles_r), nibs, ctx)
    |> List.sort(((_, grouts, _), (_, grouts', _)) =>
         Grouts.Frame.(Int.compare(size(grouts), size(grouts')))
       )
    |> ListUtil.hd_opt
    |> OptUtil.get_or_fail("Segment.connect: expected at least one remolding");
  (insertion, Frame.(concat([of_grouts(grouts), of_tiles(tiles)])));
};
