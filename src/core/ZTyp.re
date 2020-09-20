module ZTile = {
  type tile = HTyp.Tile.t;

  type s = ZList.t(option(t), tile)
  and t = Tile.t(zoperand, zpreop, zpostop, zbinop)
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
    | (Operand(ParenZ_body(zbody1)), Operand(ParenZ_body(zbody2))) =>
      compare_s(zbody1, zbody2)
    | (PreOp(_), PreOp(_)) => raise(Void_ZPreOp)
    | (PostOp(_), PostOp(_)) => raise(Void_ZPostOp)
    | (BinOp(_), BinOp(_)) => raise(Void_ZBinOp)
    | _ => raise(Invalid_argument("ZTyp.ZTile.compare"))
    };

  let erase = (~erase_s: s => list(tile), ztile: t): tile =>
    switch (ztile) {
    | Operand(ParenZ_body(zbody)) => Operand(Paren(erase_s(zbody)))
    | PreOp(_) => raise(Void_ZPreOp)
    | PostOp(_) => raise(Void_ZPostOp)
    | BinOp(_) => raise(Void_ZBinOp)
    };

  let enter_from_left: tile => option(t) =
    fun
    | Operand(OperandHole | Num) => None
    | Operand(Paren(body)) =>
      Some(Operand(ParenZ_body(ZTiles.place_after(body))))
    | PreOp(_) => raise(HTyp.Tile.Void_PreOp)
    | PostOp(_) => raise(HTyp.Tile.Void_PostOp)
    | BinOp(Arrow | OperatorHole) => None;
  let enter_from_right: tile => option(t) =
    fun
    | Operand(OperandHole | Num) => None
    | Operand(Paren(body)) =>
      Some(Operand(ParenZ_body(ZTiles.place_before(body))))
    | PreOp(_) => raise(HTyp.Tile.Void_PreOp)
    | PostOp(_) => raise(HTyp.Tile.Void_PostOp)
    | BinOp(Arrow | OperatorHole) => None;

  let opt_map =
      (
        ~opt_map_s: ((list(tile), list(tile)) => option(s), s) => option(s),
        f: (list(tile), list(tile)) => option(s),
        ztile: t,
      )
      : option(t) =>
    switch (ztile) {
    | Operand(ParenZ_body(zbody)) =>
      opt_map_s(f, zbody)
      |> Option.map(zbody => Tile.Operand(ParenZ_body(zbody)))
    | PreOp(_) => raise(Void_ZPreOp)
    | PostOp(_) => raise(Void_ZPostOp)
    | BinOp(_) => raise(Void_ZBinOp)
    };

  let remove =
      (~remove_s: (s, s) => option((list(tile), list(tile))), l: t, r: t)
      : option((list(tile), tile)) =>
    switch (l, r) {
    | (Operand(ParenZ_body(zbody_l)), Operand(ParenZ_body(zbody_r))) =>
      remove_s(zbody_l, zbody_r)
      |> Option.map(((removed, body)) =>
           (removed, Tile.Operand(HTyp.Tile.Paren(body)))
         )
    | (PreOp(_), _)
    | (_, PreOp(_)) => raise(Void_ZPreOp)
    | (PostOp(_), _)
    | (_, PostOp(_)) => raise(Void_ZPostOp)
    | (BinOp(_), _)
    | (_, BinOp(_)) => raise(Void_ZBinOp)
    };

  let restructure =
      (~restructure_s: (s, s, s) => option(s), l: t, r: t, target: t)
      : option(t) =>
    switch (l, r, target) {
    | (
        Operand(ParenZ_body(zbody_l)),
        Operand(ParenZ_body(zbody_r)),
        Operand(ParenZ_body(zbody_target)),
      ) =>
      restructure_s(zbody_l, zbody_r, zbody_target)
      |> Option.map(zbody => Tile.Operand(ParenZ_body(zbody)))
    | (PreOp(_), _, _)
    | (_, PreOp(_), _)
    | (_, _, PreOp(_)) => raise(Void_ZPreOp)
    | (PostOp(_), _, _)
    | (_, PostOp(_), _)
    | (_, _, PostOp(_)) => raise(Void_ZPostOp)
    | (BinOp(_), _, _)
    | (_, BinOp(_), _)
    | (_, _, BinOp(_)) => raise(Void_ZBinOp)
    };
};
include ZTiles.Make(HTyp.Tile, ZTile);

type t = ZTile.s;
