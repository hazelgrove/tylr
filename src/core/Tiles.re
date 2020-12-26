module Make =
       (T: Tile.S)
       : {
         let mk_hole: unit => T.s;
         let dummy_hole: T.s;

         let fix_empty_holes_2: (T.s, T.s) => (T.s, T.s);
         let fix_empty_holes_3: (T.s, T.s, T.s) => (T.s, T.s, T.s);

         [@deriving sexp]
         type root =
           Tile.t(T.op, (T.pre, T.s), (T.s, T.post), (T.s, T.bin, T.s));
         let root: T.s => root;
         let nth_root: (int, T.s) => Util.ZList.t(root, T.t);
         let is_root: (int, T.s) => bool;
       } => {
  open Util;

  let mk_hole = (): T.s => [Tile.Op(T.mk_op_hole())];
  let dummy_hole = mk_hole();

  let fix_empty_holes_between = (prefix: T.s, suffix: T.s): (T.s, T.s) => {
    switch (ListUtil.split_last_opt(prefix), suffix) {
    | (None, _)
    | (_, []) => (prefix, suffix)
    | (Some((leading, last)), [first, ...trailing]) =>
      switch (last, first) {
      | (Op(op), Bin(bin))
      | (Bin(bin), Op(op)) when T.is_op_hole(op) && T.is_bin_hole(bin) => (
          leading,
          trailing,
        )
      | (Op(op), Op(_) | Pre(_)) when T.is_op_hole(op) => (leading, suffix)
      | (Op(_) | Post(_), Op(op)) when T.is_op_hole(op) => (
          prefix,
          trailing,
        )
      | (Op(_) | Post(_), Op(_) | Pre(_)) => (
          prefix,
          [Bin(T.mk_bin_hole()), ...suffix],
        )
      | (Bin(bin), Bin(_) | Post(_)) when T.is_bin_hole(bin) => (
          leading,
          suffix,
        )
      | (Bin(_) | Pre(_), Bin(bin)) when T.is_bin_hole(bin) => (
          prefix,
          trailing,
        )
      | (Bin(_) | Pre(_), Bin(_) | Post(_)) => (
          prefix,
          [Op(T.mk_op_hole()), ...suffix],
        )
      | _ => (prefix, suffix)
      }
    };
  };

  let fix_empty_holes = (tss: list(T.s)): list(T.s) => {
    let rec fix = (ts: T.s, tss: list(T.s)): list(T.s) => {
      let skip_empty = (ts, tss) => {
        let (ts, tss) = ListUtil.split_first(fix(ts, tss));
        [ts, [], ...tss];
      };
      switch (tss) {
      | [] => [ts]
      | [[], ...tss] => skip_empty(ts, tss)
      | [[_, ..._] as ts', ...tss] =>
        let (ts, ts') = fix_empty_holes_between(ts, ts');
        switch (ts') {
        | [] => skip_empty(ts, tss)
        | [_, ..._] => [ts, ts', ...tss]
        };
      };
    };
    List.fold_right(fix, tss, []);
  };

  let fix_empty_holes_2 = (ts1, ts2) =>
    switch (fix_empty_holes([ts1, ts2])) {
    | [ts1, ts2] => (ts1, ts2)
    | _ => failwith("fix_empty_holes expected to preserve length")
    };
  let fix_empty_holes_3 = (ts1, ts2, ts3) =>
    switch (fix_empty_holes([ts1, ts2, ts3])) {
    | [ts1, ts2, ts3] => (ts1, ts2, ts3)
    | _ => failwith("fix_empty_holes expected to preserve length")
    };

  module Sk = Skel.Make(T);

  [@deriving sexp]
  type root = Tile.t(T.op, (T.pre, T.s), (T.s, T.post), (T.s, T.bin, T.s));

  let root = (tiles: T.s): root =>
    switch (Sk.mk(tiles)) {
    | Op(_) => Op(Tile.get_operand(List.hd(tiles)))
    | Pre(_) => Pre((Tile.get_preop(List.hd(tiles)), List.tl(tiles)))
    | Post(_) =>
      let (prefix, last) = ListUtil.split_last(tiles);
      Post((prefix, Tile.get_postop(last)));
    | Bin(_, n, _) =>
      let (prefix, nth, suffix) = ListUtil.split_nth(n, tiles);
      Bin((prefix, Tile.get_binop(nth), suffix));
    };

  let nth_root = (n: int, tiles: T.s): ZList.t(root, T.t) => {
    let tile = List.nth(tiles, n);
    let rec go: Skel.t => ZList.t(root, T.t) =
      fun
      | Op(m) =>
        n == m
          ? ZList.mk(~z=Tile.Op(Tile.get_operand(tile)), ())
          : raise(Invalid_argument("Tiles.nth_root"))
      | Pre(m, r) =>
        n == m
          ? {
            let (_, tiles_r, _) =
              ListUtil.split_sublist(m + 1, m + 1 + Skel.size(r), tiles);
            ZList.mk(~z=Tile.Pre((Tile.get_preop(tile), tiles_r)), ());
          }
          : {
            let zroot = go(r);
            {...zroot, prefix: [List.nth(tiles, m), ...zroot.prefix]};
          }
      | Post(l, m) =>
        n == m
          ? {
            let (_, tiles_l, _) =
              ListUtil.split_sublist(m - Skel.size(l), m, tiles);
            ZList.mk(~z=Tile.Post((tiles_l, Tile.get_postop(tile))), ());
          }
          : {
            let zroot = go(l);
            {...zroot, suffix: zroot.suffix @ [List.nth(tiles, m)]};
          }
      | Bin(l, m, r) => {
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
              ~z=Tile.Bin((tiles_l, Tile.get_binop(tile), tiles_r)),
              (),
            );
          };
        };
    go(Sk.mk(tiles));
  };

  let is_root = (n: int, ts: T.s) =>
    switch (root(ts)) {
    | Op(_)
    | Pre(_) => n == 0
    | Post((l, _))
    | Bin((l, _, _)) => n == List.length(l)
    };
};
