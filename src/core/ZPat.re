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

  let compare = (~compare_s: (s, s) => int, ztile1: t, ztile2: t): int =>
    switch (ztile1, ztile2) {
    | (ZOperand(ParenZ_body(zbody1)), ZOperand(ParenZ_body(zbody2))) =>
      compare_s(zbody1, zbody2)
    | (ZPreOp(_), ZPreOp(_)) => raise(Void_ZPreOp)
    | (ZPostOp(AnnZ_ann(_, zann1)), ZPostOp(AnnZ_ann(_, zann2))) =>
      ZTyp.compare(zann1, zann2)
    | (ZBinOp(_), ZBinOp(_)) => raise(Void_ZBinOp)
    | _ => raise(Invalid_argument("ZPat.ZTile.compare"))
    };

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

  let opt_map =
      (
        ~opt_map_s: ((list(tile), list(tile)) => option(s), s) => option(s),
        f: (list(tile), list(tile)) => option(s),
        ztile: t,
      )
      : option(t) =>
    switch (ztile) {
    | ZOperand(ParenZ_body(zbody)) =>
      opt_map_s(f, zbody)
      |> Option.map(zbody => ZTile.ZOperand(ParenZ_body(zbody)))
    | ZPreOp(_) => raise(Void_ZPreOp)
    | ZPostOp(AnnZ_ann(_)) => None
    | ZBinOp(_) => raise(Void_ZBinOp)
    };

  let remove =
      (~remove_s: (s, s) => option((list(tile), list(tile))), l: t, r: t)
      : option((list(tile), tile)) =>
    switch (l, r) {
    | (ZOperand(ParenZ_body(zbody_l)), ZOperand(ParenZ_body(zbody_r))) =>
      remove_s(zbody_l, zbody_r)
      |> Option.map(((removed, body)) =>
           (removed, Tile.Operand(HPat.Tile.Paren(body)))
         )
    | (ZPreOp(_), _)
    | (_, ZPreOp(_)) => raise(Void_ZPreOp)
    | (ZPostOp(AnnZ_ann(_)), ZPostOp(AnnZ_ann(_))) => None
    | (ZBinOp(_), _)
    | (_, ZBinOp(_)) => raise(Void_ZBinOp)
    | _ =>
      raise(
        Invalid_argument(
          "ZPat.ZTile.remove: expected arguments to have same erasure",
        ),
      )
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
    | (
        ZPostOp(AnnZ_ann(status, zann_l)),
        ZPostOp(AnnZ_ann(_, zann_r)),
        ZPostOp(AnnZ_ann(_, zann_target)),
      ) =>
      ZTyp.restructure(zann_l, zann_r, zann_target)
      |> Option.map(zann => ZTile.ZPostOp(AnnZ_ann(status, zann)))
    | (ZBinOp(_), _, _)
    | (_, ZBinOp(_), _)
    | (_, _, ZBinOp(_)) => raise(Void_ZBinOp)
    | _ =>
      raise(
        Invalid_argument(
          "ZPat.ZTile.restructure: expected arguments to have same erasure",
        ),
      )
    };
};
include ZTiles.Make(HPat.Tile, ZTile);

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
