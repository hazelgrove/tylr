module ZTile = {
  type tile = HTyp.Tile.t;

  type s = ZList.t(option(t), tile)
  and t = ZTile.t(zoperand, zpreop, zpostop, zbinop)
  and zoperand =
    | ParenZ_body(s)
  and zpreop = unit // empty
  and zpostop = unit // empty
  and zbinop = unit; // empty

  exception Void_ZPreOp;
  exception Void_ZPostOp;
  exception Void_ZBinOp;

  let erase = (~erase_s: s => list(tile), ztile: t): tile =>
    switch (ztile) {
    | ZOperand(ParenZ_body(zbody)) => Operand(Paren(erase_s(zbody)))
    | ZPreOp(_) => raise(Void_ZPreOp)
    | ZPostOp(_) => raise(Void_ZPostOp)
    | ZBinOp(_) => raise(Void_ZBinOp)
    };

  let enter_from_left: tile => option(t) =
    fun
    | Operand(OperandHole | Num) => None
    | Operand(Paren(body)) =>
      Some(ZOperand(ParenZ_body(ZTiles.place_after(body))))
    | PreOp(_) => raise(HTyp.Tile.Void_PreOp)
    | PostOp(_) => raise(HTyp.Tile.Void_PostOp)
    | BinOp(Arrow | OperatorHole) => None;
  let enter_from_right: tile => option(t) =
    fun
    | Operand(OperandHole | Num) => None
    | Operand(Paren(body)) =>
      Some(ZOperand(ParenZ_body(ZTiles.place_before(body))))
    | PreOp(_) => raise(HTyp.Tile.Void_PreOp)
    | PostOp(_) => raise(HTyp.Tile.Void_PostOp)
    | BinOp(Arrow | OperatorHole) => None;
};
include ZTiles.Util(HTyp.Tile, ZTile);

type t = ZTile.s;
