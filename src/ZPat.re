module ZTile = {
  type tile = HPat.Tile.t;

  type s = ZList.t(option(t), tile)
  and t = ZTile.t(zoperand, zpreop, zpostop, zbinop)
  and zoperand =
    | ParenZ_body(s)
  and zpreop = unit // empty
  and zpostop =
    | AnnZ_ann(HoleStatus.t, ZTyp.t)
  and zbinop = unit; // empty

  exception Void_ZPreOp;
  exception Void_ZBinOp;

  let erase = (~erase_s: s => list(tile), ztile: t): tile =>
    switch (ztile) {
    | ZOperand(ParenZ_body(zbody)) => Operand(Paren(erase_s(zbody)))
    | ZPreOp(_) => raise(Void_ZPreOp)
    | ZPostOp(AnnZ_ann(status, zann)) =>
      PostOp(Ann(status, ZTyp.erase(zann)))
    | ZBinOp(_) => raise(Void_ZBinOp)
    };

  let enter_from_left: tile => option(t) =
    fun
    | Operand(OperandHole | Var(_)) => None
    | Operand(Paren(body)) =>
      Some(ZOperand(ParenZ_body(ZTiles.place_before(body))))
    | PreOp(_) => raise(HPat.Tile.Void_PreOp)
    | PostOp(Ann(status, ann)) =>
      Some(ZPostOp(AnnZ_ann(status, ZTiles.place_before(ann))))
    | BinOp(OperatorHole) => None;
  let enter_from_right: tile => option(t) =
    fun
    | Operand(OperandHole | Var(_)) => None
    | Operand(Paren(body)) =>
      Some(ZOperand(ParenZ_body(ZTiles.place_after(body))))
    | PreOp(_) => raise(HPat.Tile.Void_PreOp)
    | PostOp(Ann(status, ann)) =>
      Some(ZPostOp(AnnZ_ann(status, ZTiles.place_after(ann))))
    | BinOp(OperatorHole) => None;
};
include ZTiles.Util(HPat.Tile, ZTile);

type t = ZTile.s;

let mk =
    (
      ~prefix: list(HPat.Tile.t)=[],
      ~z: option(ZTile.t)=?,
      ~suffix: list(HPat.Tile.t)=[],
      (),
    )
    : t =>
  ZList.{prefix, z, suffix};

let rec set_hole_status = (status: HoleStatus.t): (t => t) =>
  map_root(
    ~operand=HPat.set_hole_status_operand(status),
    ~preop=HPat.set_hole_status_preop(status),
    ~postop=HPat.set_hole_status_postop(status),
    ~binop=HPat.set_hole_status_binop(status),
    ~zoperand=set_hole_status_zoperand(status),
    ~zpreop=set_hole_status_zpreop(status),
    ~zpostop=set_hole_status_zpostop(status),
    ~zbinop=set_hole_status_zbinop(status),
  )
and set_hole_status_zoperand = status =>
  fun
  | ParenZ_body(zp) => ParenZ_body(set_hole_status(status, zp))
and set_hole_status_zpreop = _ =>
  fun
  | _ => raise(ZTile.Void_ZPreOp)
and set_hole_status_zpostop = status =>
  fun
  | AnnZ_ann(_, ann) => AnnZ_ann(status, ann)
and set_hole_status_zbinop = _ =>
  fun
  | _ => raise(ZTile.Void_ZBinOp);
