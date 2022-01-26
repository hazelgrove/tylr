open Tile;
open Util;

[@deriving sexp]
type t = s;

module Placeholders = {
  type t = list(Tile.Placeholder.t);

  let adjust = (_: Nibs.t, _: t): t => failwith("Tiles.Placeholders.adjust");
};

type hd = Placeholders.t;
type tl = Util.Baba.t(Tile.t, Placeholders.t);

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

module Frame = {
  type affix_tl = tl;
  type affix_hd = hd;
  type affix = t;
  type t = (affix, affix);

  let near_nib = (d: Direction.t, tl: affix_tl, back_nib: Nib.t) =>
    switch (tl) {
    | [] => back_nib
    | [(tile, _), ..._] =>
      Direction.(choose(toggle(d), Tile.Mold.nibs(tile.mold)))
    };

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

  let reshape_affix_tl =
      (d: Direction.t, tl: affix_tl, back_nib: Nib.t): list(affix_tl) => {
    let fold = ((tile, ps), (tl: affix_tl, k: unit => list(affix_tl))) => {
      let tl = [(tile, ps), ...tl];
      let k = () =>
        switch (Tile.reshape(tile)) {
        | [_] => [tl] // short-circuit reshaping when only one option
        | reshapings =>
          open ListUtil.Syntax;
          let* reshaped_tile = reshapings;
          let+ reshaped_tl = k();
          let adjusted_ps = {
            let far_nib = near_nib(d, reshaped_tl, back_nib);
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

  let reshape_affix =
      (d: Direction.t, (hd, tl): affix, back_nib: Nib.t)
      : (affix_hd, list(affix_tl)) => (
    hd,
    reshape_affix_tl(d, tl, back_nib),
  );
};
