type t('zoperand, 'zpreop, 'zpostop, 'zbinop) =
  | ZOperand('zoperand)
  | ZPreOp('zpreop)
  | ZPostOp('zpostop)
  | ZBinOp('zbinop);

let get_zoperand =
  fun
  | ZOperand(zoperand) => zoperand
  | _ => raise(Invalid_argument("ZTile.get_zoperand"));
let get_zpreop =
  fun
  | ZPreOp(zpreop) => zpreop
  | _ => raise(Invalid_argument("ZTile.get_zpreop"));
let get_zpostop =
  fun
  | ZPostOp(zpostop) => zpostop
  | _ => raise(Invalid_argument("ZTile.get_zpostop"));
let get_zbinop =
  fun
  | ZBinOp(zbinop) => zbinop
  | _ => raise(Invalid_argument("ZTile.get_zbinop"));

let map =
    (
      ~zoperand: 'zoperand => 'zoperand,
      ~zpreop: 'zpreop => 'zpreop,
      ~zpostop: 'zpostop => 'zpostop,
      ~zbinop: 'zbinop => 'zbinop,
      ztile: t('zoperand, 'zpreop, 'zpostop, 'zbinop),
    )
    : t('zoperand, 'zpreop, 'zpostop, 'zbinop) =>
  switch (ztile) {
  | ZOperand(z) => ZOperand(zoperand(z))
  | ZPreOp(z) => ZPreOp(zpreop(z))
  | ZPostOp(z) => ZPostOp(zpostop(z))
  | ZBinOp(z) => ZBinOp(zbinop(z))
  };

module type S = {
  type tile;

  type zoperand;
  type zpreop;
  type zpostop;
  type zbinop;

  type nonrec t = t(zoperand, zpreop, zpostop, zbinop);
  type s = ZList.t(option(t), tile);

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
