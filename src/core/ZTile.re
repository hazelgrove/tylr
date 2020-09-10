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
