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

  let compare = (~compare_s: (s, s) => int, ztile1: t, ztile2: t): int =>
    switch (ztile1, ztile2) {
    | (ZOperand(ParenZ_body(zbody1)), ZOperand(ParenZ_body(zbody2))) =>
      compare_s(zbody1, zbody2)
    | (ZPreOp(_), ZPreOp(_)) => raise(Void_ZPreOp)
    | (ZPostOp(_), ZPostOp(_)) => raise(Void_ZPostOp)
    | (ZBinOp(_), ZBinOp(_)) => raise(Void_ZBinOp)
    | _ => raise(Invalid_argument("ZTyp.ZTile.compare"))
    };

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

  let insert =
      (
        ~insert_s: (list(tile), s) => option(s),
        tiles: list(tile),
        ztile: t,
      )
      : option(t) =>
    switch (ztile) {
    | ZOperand(ParenZ_body(zbody)) =>
      insert_s(tiles, zbody)
      |> Option.map(zbody => ZTile.ZOperand(ParenZ_body(zbody)))
    | ZPreOp(_) => raise(Void_ZPreOp)
    | ZPostOp(_) => raise(Void_ZPostOp)
    | ZBinOp(_) => raise(Void_ZBinOp)
    };

  let remove =
      (~remove_s: (s, s) => option((list(tile), list(tile))), l: t, r: t)
      : option((list(tile), tile)) =>
    switch (l, r) {
    | (ZOperand(ParenZ_body(zbody_l)), ZOperand(ParenZ_body(zbody_r))) =>
      remove_s(zbody_l, zbody_r)
      |> Option.map(((removed, body)) =>
           (removed, Tile.Operand(HTyp.Tile.Paren(body)))
         )
    | (ZPreOp(_), _)
    | (_, ZPreOp(_)) => raise(Void_ZPreOp)
    | (ZPostOp(_), _)
    | (_, ZPostOp(_)) => raise(Void_ZPostOp)
    | (ZBinOp(_), _)
    | (_, ZBinOp(_)) => raise(Void_ZBinOp)
    };

  let restructure =
      (~restructure_s: (s, s, s) => option(s), l: t, r: t, target: t)
      : option(t) =>
    switch (l, r, target) {
    | (
        ZOperand(ParenZ_body(zbody_l)),
        ZOperand(ParenZ_body(zbody_r)),
        ZOperand(ParenZ_body(zbody_target)),
      ) =>
      restructure_s(zbody_l, zbody_r, zbody_target)
      |> Option.map(zbody => ZTile.ZOperand(ParenZ_body(zbody)))
    | (ZPreOp(_), _, _)
    | (_, ZPreOp(_), _)
    | (_, _, ZPreOp(_)) => raise(Void_ZPreOp)
    | (ZPostOp(_), _, _)
    | (_, ZPostOp(_), _)
    | (_, _, ZPostOp(_)) => raise(Void_ZPostOp)
    | (ZBinOp(_), _, _)
    | (_, ZBinOp(_), _)
    | (_, _, ZBinOp(_)) => raise(Void_ZBinOp)
    };
};
include ZTiles.Util(HTyp.Tile, ZTile);

type t = ZTile.s;
