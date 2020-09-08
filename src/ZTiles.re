module type ZTILE = {
  type tile;

  type s = ZList.t(option(t), tile)
  and t = ZTile.t(zoperand, zpreop, zpostop, zbinop)
  and zoperand
  and zpreop
  and zpostop
  and zbinop;

  let compare: (~compare_s: (s, s) => int, t, t) => int;

  let erase: (~erase_s: s => list(tile), t) => tile;

  let enter_from_left: tile => option(t);
  let enter_from_right: tile => option(t);

  let opt_map:
    (
      ~opt_map_s: ((list(tile), list(tile)) => option(s), s) => option(s),
      (list(tile), list(tile)) => option(s),
      t
    ) =>
    option(t);

  let remove:
    (~remove_s: (s, s) => option((list(tile), list(tile))), t, t) =>
    option((list(tile), tile));

  let restructure:
    (~restructure_s: (s, s, s) => option(s), t, t, t) => option(t);
};

let wrap = ztile => ZList.{prefix: [], z: Some(ztile), suffix: []};

let place_before = tiles => ZList.{prefix: [], z: None, suffix: tiles};
let place_after = tiles => ZList.{prefix: tiles, z: None, suffix: []};

module Util =
       (T: Tiles.TILE, Z: ZTILE with type tile = T.t)
       : {
         let mk: (~prefix: T.s=?, ~z: Z.t=?, ~suffix: T.s=?, unit) => Z.s;

         let z_index: Z.s => int;

         let compare: (Z.s, Z.s) => int;

         let erase: Z.s => T.s;

         let move_left: (T.s, T.s) => option(Z.s);
         let move_right: (T.s, T.s) => option(Z.s);

         type root =
           | OperandZ(Z.zoperand)
           | PreOpZ_op(Z.zpreop, T.s)
           | PreOpZ_arg(T.preop, Z.s)
           | PostOpZ_op(T.s, Z.zpostop)
           | PostOpZ_arg(Z.s, T.postop)
           | BinOpZ_op(T.s, Z.zbinop, T.s)
           | BinOpZ_larg(Z.s, T.binop, T.s)
           | BinOpZ_rarg(T.s, T.binop, Z.s);

         let root: Z.s => option(root);

         let map_root:
           (
             ~operand: T.operand => T.operand,
             ~preop: T.preop => T.preop,
             ~postop: T.postop => T.postop,
             ~binop: T.binop => T.binop,
             ~zoperand: Z.zoperand => Z.zoperand,
             ~zpreop: Z.zpreop => Z.zpreop,
             ~zpostop: Z.zpostop => Z.zpostop,
             ~zbinop: Z.zbinop => Z.zbinop,
             Z.s
           ) =>
           Z.s;

         let opt_map: ((T.s, T.s) => option(Z.s), Z.s) => option(Z.s);

         let restructure: (Z.s, Z.s, Z.s) => option(Z.s);
       } => {
  let mk = (~prefix: T.s=[], ~z: option(Z.t)=?, ~suffix: T.s=[], ()): Z.s =>
    ZList.{prefix, z, suffix};

  let z_index = (ztiles: Z.s) => List.length(ztiles.prefix);

  let rec erase = (ztiles: Z.s) =>
    switch (ztiles.z) {
    | None => ztiles.prefix @ ztiles.suffix
    | Some(ztile) =>
      ztiles.prefix @ [Z.erase(~erase_s=erase, ztile), ...ztiles.suffix]
    };

  let move_left = (prefix, suffix) =>
    ListUtil.split_last_opt(prefix)
    |> Option.map(((prefix, tile)) =>
         switch (Z.enter_from_right(tile)) {
         | None => ZList.{prefix, z: None, suffix: [tile, ...suffix]}
         | Some(_) as z => {prefix, z, suffix}
         }
       );

  let move_right = (prefix, suffix) =>
    switch (suffix) {
    | [] => None
    | [tile, ...suffix] =>
      Some(
        switch (Z.enter_from_left(tile)) {
        | None => ZList.{prefix: prefix @ [tile], z: None, suffix}
        | Some(_) as z => {prefix, z, suffix}
        },
      )
    };

  type root =
    | OperandZ(Z.zoperand)
    | PreOpZ_op(Z.zpreop, T.s)
    | PreOpZ_arg(T.preop, Z.s)
    | PostOpZ_op(T.s, Z.zpostop)
    | PostOpZ_arg(Z.s, T.postop)
    | BinOpZ_op(T.s, Z.zbinop, T.s)
    | BinOpZ_larg(Z.s, T.binop, T.s)
    | BinOpZ_rarg(T.s, T.binop, Z.s);

  module TUtil = Tiles.Util(T);

  let root = ({prefix, z, suffix}: Z.s): option(root) =>
    z
    |> Option.map(z => {
         let tiles = erase({prefix, z: Some(z), suffix});
         let z_index = List.length(prefix);
         switch (TUtil.root(tiles)) {
         | Operand(_) => OperandZ(ZTile.get_zoperand(z))
         | PreOp(preop, tiles) =>
           let op_index = 0;
           z_index == op_index
             ? PreOpZ_op(ZTile.get_zpreop(z), tiles)
             : PreOpZ_arg(
                 preop,
                 {prefix: List.tl(prefix), z: Some(z), suffix},
               );
         | PostOp(tiles, postop) =>
           let op_index = List.length(tiles);
           z_index == op_index
             ? PostOpZ_op(tiles, ZTile.get_zpostop(z))
             : PostOpZ_arg(
                 {prefix, z: Some(z), suffix: ListUtil.leading(suffix)},
                 postop,
               );
         | BinOp(tiles_l, binop, tiles_r) =>
           let op_index = List.length(tiles_l);
           if (z_index < op_index) {
             let (prefix, _, suffix) = ListUtil.split_nth(z_index, tiles_l);
             BinOpZ_larg({prefix, z: Some(z), suffix}, binop, tiles_r);
           } else if (z_index > op_index) {
             let (prefix, _, suffix) =
               ListUtil.split_nth(z_index - (op_index + 1), tiles_r);
             BinOpZ_rarg(tiles_l, binop, {prefix, z: Some(z), suffix});
           } else {
             BinOpZ_op(tiles_l, ZTile.get_zbinop(z), tiles_r);
           };
         };
       });

  let map_root =
      (
        ~operand: T.operand => T.operand,
        ~preop: T.preop => T.preop,
        ~postop: T.postop => T.postop,
        ~binop: T.binop => T.binop,
        ~zoperand: Z.zoperand => Z.zoperand,
        ~zpreop: Z.zpreop => Z.zpreop,
        ~zpostop: Z.zpostop => Z.zpostop,
        ~zbinop: Z.zbinop => Z.zbinop,
        {prefix, suffix, _} as ztiles: Z.s,
      )
      : Z.s => {
    switch (root(ztiles)) {
    | None =>
      let n = List.length(prefix);
      let (prefix, suffix) =
        prefix
        @ suffix
        |> TUtil.map_root(~operand, ~preop, ~postop, ~binop)
        |> ListUtil.split_n(n);
      {prefix, z: None, suffix};
    | Some(root) =>
      switch (root) {
      | OperandZ(z) => {
          prefix: [],
          z: Some(ZOperand(zoperand(z))),
          suffix: [],
        }
      | PreOpZ_op(z, ts) => {
          prefix: [],
          z: Some(ZPreOp(zpreop(z))),
          suffix: ts,
        }
      | PreOpZ_arg(t, {prefix, z, suffix}) => {
          prefix: [PreOp(preop(t)), ...prefix],
          z,
          suffix,
        }
      | PostOpZ_op(ts, z) => {
          prefix: ts,
          z: Some(ZPostOp(zpostop(z))),
          suffix: [],
        }
      | PostOpZ_arg({prefix, z, suffix}, t) => {
          prefix,
          z,
          suffix: suffix @ [PostOp(postop(t))],
        }
      | BinOpZ_op(tsl, z, tsr) => {
          prefix: tsl,
          z: Some(ZBinOp(zbinop(z))),
          suffix: tsr,
        }
      | BinOpZ_larg({prefix, z, suffix}, t, ts) => {
          prefix,
          z,
          suffix: suffix @ [BinOp(binop(t)), ...ts],
        }
      | BinOpZ_rarg(ts, t, {prefix, z, suffix}) => {
          prefix: ts @ [BinOp(binop(t)), ...prefix],
          z,
          suffix,
        }
      }
    };
  };

  let rec compare = (ztiles1: Z.s, ztiles2: Z.s): int => {
    assert(erase(ztiles1) == erase(ztiles2));
    let n1 = z_index(ztiles1);
    let n2 = z_index(ztiles2);
    switch (ztiles1.z, ztiles2.z) {
    | (None, None) => Int.compare(n1, n2)
    | (None, Some(_)) => n1 <= n2 ? (-1) : 1
    | (Some(_), None) => n1 < n2 ? (-1) : 1
    | (Some(ztile1), Some(ztile2)) =>
      if (n1 < n2) {
        (-1);
      } else if (n1 > n2) {
        1;
      } else {
        Z.compare(~compare_s=compare, ztile1, ztile2);
      }
    };
  };

  let rec opt_map =
          (f: (T.s, T.s) => option(Z.s), {prefix, z, suffix}: Z.s)
          : option(Z.s) =>
    switch (z) {
    | None => f(prefix, suffix)
    | Some(ztile) =>
      Z.opt_map(~opt_map_s=opt_map, f, ztile)
      |> Option.map(ztile => ZList.{prefix, z: Some(ztile), suffix})
    };

  let insert = (tiles: T.s): (Z.s => option(Z.s)) =>
    opt_map((prefix, suffix) =>
      Some(mk(~prefix, ~suffix=tiles @ suffix, ()))
    );

  let rec remove = (l: Z.s, r: Z.s): option((T.s, T.s)) => {
    assert(erase(l) == erase(r));
    if (compare(l, r) > 0) {
      remove(r, l);
    } else {
      let n_l = z_index(l);
      let n_r = z_index(r);
      let tiles = erase(l);
      switch (l.z, r.z) {
      | (None, None) =>
        let (prefix, removed, suffix) =
          ListUtil.split_sublist(n_l, n_r, tiles);
        Some((removed, prefix @ suffix));
      | (Some(_), None)
      | (None, Some(_)) => None
      | (Some(ztile_l), Some(ztile_r)) =>
        n_l != n_r
          ? None
          : Z.remove(~remove_s=remove, ztile_l, ztile_r)
            |> Option.map(((removed, tile)) =>
                 (removed, l.prefix @ [tile, ...l.suffix])
               )
      };
    };
  };

  let rec restructure = (l: Z.s, r: Z.s, target: Z.s): option(Z.s) =>
    if (compare(l, r) > 0) {
      restructure(r, l, target);
    } else if (compare(l, target) < 0 && compare(target, r) < 0) {
      None;
    } else {
      let n_l = z_index(l);
      let n_r = z_index(r);
      let n_target = z_index(target);
      switch (l.z, r.z, target.z) {
      | (Some(_), None, None)
      | (None, Some(_), Some(_)) => restructure(r, target, l)
      | (None, Some(_), None)
      | (Some(_), None, Some(_)) => restructure(target, l, r)
      | (None, None, None) =>
        if (compare(target, l) <= 0) {
          let (s1, selected, s2) =
            ListUtil.split_sublist(
              n_l - n_target,
              n_r - n_target,
              target.suffix,
            );
          Some(mk(~prefix=target.prefix, ~suffix=selected @ s1 @ s2, ()));
        } else {
          // compare(r, target) <= 0
          let (p1, removed, p2) =
            ListUtil.split_sublist(n_l, n_r, target.prefix);
          Some(mk(~prefix=p1 @ p2, ~suffix=removed @ target.suffix, ()));
        }
      | (None, None, Some(_)) =>
        if (compare(target, l) < 0) {
          let (s1, selected, s2) =
            ListUtil.split_sublist(
              n_l - (n_target + 1),
              n_r - (n_target + 1),
              target.suffix,
            );
          insert(selected, target)
          |> Option.map(inserted => ZList.{...inserted, suffix: s1 @ s2});
        } else {
          let (p1, selected, p2) =
            ListUtil.split_sublist(n_l, n_r, target.prefix);
          insert(selected, target)
          |> Option.map(inserted => ZList.{...inserted, prefix: p1 @ p2});
        }
      | (Some(ztile_l), Some(ztile_r), None) =>
        if (n_l != n_r) {
          None;
        } else {
          Z.remove(~remove_s=remove, ztile_l, ztile_r)
          |> Option.map(((removed, remainder)) =>
               compare(target, l) < 0
                 ? mk(
                     ~prefix=target.prefix,
                     ~suffix=
                       removed
                       @ ListUtil.put_nth(
                           n_l - n_target,
                           remainder,
                           target.suffix,
                         ),
                     (),
                   )
                 : mk(
                     ~prefix=ListUtil.put_nth(n_l, remainder, target.prefix),
                     ~suffix=removed @ target.suffix,
                     (),
                   )
             );
        }
      | (Some(ztile_l), Some(ztile_r), Some(ztile_target)) =>
        if (n_l == n_r && n_r == n_target) {
          Z.restructure(
            ~restructure_s=restructure,
            ztile_l,
            ztile_r,
            ztile_target,
          )
          |> Option.map(ztile => {...target, z: Some(ztile)});
        } else if (n_l == n_r) {
          Option.bind(
            Z.remove(~remove_s=remove, ztile_l, ztile_r),
            ((removed, remainder)) =>
            insert(removed, target)
            |> Option.map((inserted: Z.s) =>
                 compare(target, l) < 0
                   ? {
                     ...inserted,
                     suffix:
                       ListUtil.put_nth(
                         n_l - (n_target + 1),
                         remainder,
                         target.suffix,
                       ),
                   }
                   : {
                     ...inserted,
                     prefix: ListUtil.put_nth(n_l, remainder, target.prefix),
                   }
               )
          );
        } else if (n_r == n_target) {
          restructure(r, target, l);
        } else if (n_target == n_l) {
          restructure(target, l, r);
        } else {
          None;
        }
      };
    };
};
