module Make =
       (T: Tile.S)
       : {
         let mk_hole: unit => T.s;
         let dummy_hole: T.s;

         let fix_empty_holes: (T.s, T.s) => (T.s, T.s);

         [@deriving sexp]
         type root =
           Tile.t(
             T.operand,
             (T.preop, T.s),
             (T.s, T.postop),
             (T.s, T.binop, T.s),
           );
         let root: T.s => root;
         let nth_root: (int, T.s) => Util.ZList.t(root, T.t);
         let is_root: (int, T.s) => bool;
       } => {
  open Util;

  let mk_hole = (): T.s => [T.mk_operand_hole()];
  let dummy_hole = mk_hole();

  let fix_empty_holes_between = (prefix: T.s, suffix: T.s): (T.s, T.s) => {
    switch (ListUtil.split_last_opt(prefix), suffix) {
    | (None, _)
    | (_, []) => (prefix, suffix)
    | (Some((leading, last)), [first, ...trailing]) =>
      switch (last, first) {
      | (Operand(_) | PostOp(_), Operand(_) | PreOp(_)) =>
        if (T.is_operand_hole(first)) {
          (prefix, trailing);
        } else if (T.is_operand_hole(last)) {
          (leading, suffix);
        } else {
          (prefix, [T.mk_operator_hole(), ...suffix]);
        }
      | (BinOp(_) | PreOp(_), BinOp(_) | PostOp(_)) =>
        if (T.is_operator_hole(first)) {
          (prefix, trailing);
        } else if (T.is_operator_hole(last)) {
          (leading, suffix);
        } else {
          (prefix, [T.mk_operand_hole(), ...suffix]);
        }
      | _ =>
        T.is_operand_hole(last)
        && T.is_operator_hole(first)
        || T.is_operator_hole(last)
        && T.is_operand_hole(first)
          ? (leading, trailing) : (prefix, suffix)
      }
    };
  };

  let fix_empty_holes_left = (ts: T.s): T.s =>
    switch (ts) {
    | [] => [T.mk_operand_hole()]
    | [t, ...ts] =>
      let left_cap =
        switch (t) {
        | _ when T.is_operator_hole(t) => []
        | Operand(_)
        | PreOp(_) => [t]
        | PostOp(_)
        | BinOp(_) => [T.mk_operand_hole(), t]
        };
      left_cap @ ts;
    };

  let fix_empty_holes_right = (ts: T.s): T.s =>
    switch (ListUtil.split_last_opt(ts)) {
    | None => [T.mk_operand_hole()]
    | Some((ts, t)) =>
      let right_cap =
        switch (t) {
        | _ when T.is_operator_hole(t) => []
        | Operand(_)
        | PostOp(_) => [t]
        | PreOp(_)
        | BinOp(_) => [t, T.mk_operand_hole()]
        };
      ts @ right_cap;
    };

  // Assumes prefix and suffix are internally structurally correct,
  // only fixes things at prefix/suffix interface and at ends.
  // Puts new empty holes on suffix.
  let fix_empty_holes = (prefix: T.s, suffix: T.s): (T.s, T.s) => {
    let (prefix, suffix) = fix_empty_holes_between(prefix, suffix);
    switch (prefix, suffix) {
    | ([], _) =>
      let suffix = fix_empty_holes_left(fix_empty_holes_right(suffix));
      (prefix, suffix);
    | (_, []) =>
      let prefix = fix_empty_holes_right(fix_empty_holes_left(prefix));
      (prefix, suffix);
    | _ =>
      let prefix = fix_empty_holes_left(prefix);
      let suffix = fix_empty_holes_right(suffix);
      (prefix, suffix);
    };
  };

  module Sk = Skel.Make(T);

  [@deriving sexp]
  type root =
    Tile.t(
      T.operand,
      (T.preop, T.s),
      (T.s, T.postop),
      (T.s, T.binop, T.s),
    );

  let root = (tiles: T.s): root =>
    switch (Sk.mk(tiles)) {
    | Operand(_) => Operand(Tile.get_operand(List.hd(tiles)))
    | PreOp(_) => PreOp((Tile.get_preop(List.hd(tiles)), List.tl(tiles)))
    | PostOp(_) =>
      let (prefix, last) = ListUtil.split_last(tiles);
      PostOp((prefix, Tile.get_postop(last)));
    | BinOp(_, n, _) =>
      let (prefix, nth, suffix) = ListUtil.split_nth(n, tiles);
      BinOp((prefix, Tile.get_binop(nth), suffix));
    };

  let nth_root = (n: int, tiles: T.s): ZList.t(root, T.t) => {
    let tile = List.nth(tiles, n);
    let rec go: Skel.t => ZList.t(root, T.t) =
      fun
      | Operand(m) =>
        n == m
          ? ZList.mk(~z=Tile.Operand(Tile.get_operand(tile)), ())
          : raise(Invalid_argument("Tiles.nth_root"))
      | PreOp(m, r) =>
        n == m
          ? {
            let (_, tiles_r, _) =
              ListUtil.split_sublist(m + 1, m + 1 + Skel.size(r), tiles);
            ZList.mk(~z=Tile.PreOp((Tile.get_preop(tile), tiles_r)), ());
          }
          : {
            let zroot = go(r);
            {...zroot, prefix: [List.nth(tiles, m), ...zroot.prefix]};
          }
      | PostOp(l, m) =>
        n == m
          ? {
            let (_, tiles_l, _) =
              ListUtil.split_sublist(m - Skel.size(l), m, tiles);
            ZList.mk(~z=Tile.PostOp((tiles_l, Tile.get_postop(tile))), ());
          }
          : {
            let zroot = go(l);
            {...zroot, suffix: zroot.suffix @ [List.nth(tiles, m)]};
          }
      | BinOp(l, m, r) => {
          let (_, tiles_l, _) =
            ListUtil.split_sublist(m - Skel.size(l), m, tiles);
          let (_, tiles_r, _) =
            ListUtil.split_sublist(m + 1, m + 1 + Skel.size(r), tiles);
          if (n < m) {
            let zroot = go(l);
            {
              ...zroot,
              suffix: zroot.suffix @ [List.nth(tiles, m), ...tiles_r],
            };
          } else if (n > m) {
            let zroot = go(r);
            {
              ...zroot,
              prefix: tiles_l @ [List.nth(tiles, m), ...zroot.prefix],
            };
          } else {
            ZList.mk(
              ~z=Tile.BinOp((tiles_l, Tile.get_binop(tile), tiles_r)),
              (),
            );
          };
        };
    go(Sk.mk(tiles));
  };

  let is_root = (n: int, ts: T.s) =>
    switch (root(ts)) {
    | Operand(_)
    | PreOp(_) => n == 0
    | PostOp((l, _))
    | BinOp((l, _, _)) => n == List.length(l)
    };
};
