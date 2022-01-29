open Tile;
open Util;

[@deriving sexp]
type t = s;

type hd = Grouts.t;
type tl = Util.Baba.t(Tile.t, Grouts.t);

let empty = ([], []);
let rev = _ => failwith("todo Tiles.rev");
let concat = _ => failwith("todo Tiles.concat");

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

module Affix = {
  type nonrec t = t;
  type nonrec hd = hd;
  type nonrec tl = tl;

  let near_nib_tl = (d: Direction.t, tl: tl, far_nib: Nib.t) =>
    switch (tl) {
    | [] => far_nib
    | [(tile, _), ..._] =>
      Direction.(choose(toggle(d), Tile.Mold.nibs(tile.mold)))
    };
  let near_nib = (d: Direction.t, (hd, tl): t, far_nib: Nib.t) => (
    hd,
    near_nib_tl(d, tl, far_nib),
  );

  // let adjust_placeholders =
  //     (d: Direction.t, ps: Placeholders.t, far_nib: Nib.t) =>
  //   ListUtil.fold_right_map(
  //     ((tile, ps), far_nib) => {
  //       let near_nib = Tile.tip(d, tile);
  //       let ps = adjust(d, ps, (near_nib, far_nib));
  //       ((tile, ps), Tile.tip(Direction.toggle(d), tile));
  //     },
  //     tl,
  //     far_nib,
  //   );
  let adjust_placeholders = (_, _, _) => failwith("todo");

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
            let near_nib = Direction.choose(d, Tile.Mold.nibs(tile.mold));
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
};
