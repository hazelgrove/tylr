module type ZTILE = {
  type tile;

  type zoperand;
  type zpreop;
  type zpostop;
  type zbinop;

  type t = ZTile.t(zoperand, zpreop, zpostop, zbinop);
  type s = ZList.t(option(t), tile);

  let erase: (~erase_s: s => list(tile), t) => tile;

  let enter_from_left: tile => option(t);
  let enter_from_right: tile => option(t);
};

let wrap = ztile => ZList.{prefix: [], z: Some(ztile), suffix: []};

let place_before = tiles => ZList.{prefix: [], z: None, suffix: tiles};
let place_after = tiles => ZList.{prefix: tiles, z: None, suffix: []};

module Util =
       (T: Tiles.TILE, Z: ZTILE with type tile = T.t)
       : {
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
       } => {
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
         | PreOp(preop, ts) =>
           let op_index = 0;
           z_index == op_index
             ? PreOpZ_op(ZTile.get_zpreop(z), ts)
             : PreOpZ_arg(
                 preop,
                 {prefix: List.tl(prefix), z: Some(z), suffix},
               );
         | PostOp(ts, postop) =>
           let op_index = List.length(ts);
           z_index == op_index
             ? PostOpZ_op(ts, ZTile.get_zpostop(z))
             : PostOpZ_arg(
                 {prefix, z: Some(z), suffix: ListUtil.leading(suffix)},
                 postop,
               );
         | BinOp(tsl, binop, tsr) =>
           let op_index = List.length(tsl);
           if (z_index < op_index) {
             let (prefix, _, suffix) = ListUtil.split_nth(z_index, tsl);
             BinOpZ_larg({prefix, z: Some(z), suffix}, binop, tsr);
           } else if (z_index > op_index) {
             let (prefix, _, suffix) =
               ListUtil.split_nth(z_index - (op_index + 1), tsr);
             BinOpZ_rarg(tsl, binop, {prefix, z: Some(z), suffix});
           } else {
             BinOpZ_op(tsl, ZTile.get_zbinop(z), tsr);
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
};
