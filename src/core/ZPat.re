module ZTile = {
  type tile = HPat.Tile.t;

  type s = ZList.t(option(t), tile)
  and t = Tile.t(zoperand, zpreop, zpostop, zbinop)
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
    | (Operand(ParenZ_body(zbody1)), Operand(ParenZ_body(zbody2))) =>
      compare_s(zbody1, zbody2)
    | (PreOp(_), PreOp(_)) => raise(Void_ZPreOp)
    | (PostOp(AnnZ_ann(_, zann1)), PostOp(AnnZ_ann(_, zann2))) =>
      ZTyp.compare(zann1, zann2)
    | (BinOp(_), BinOp(_)) => raise(Void_ZBinOp)
    | _ => raise(Invalid_argument("ZPat.ZTile.compare"))
    };

  let erase = (~erase_s: s => list(tile), ztile: t): tile =>
    switch (ztile) {
    | Operand(ParenZ_body(zbody)) => Operand(Paren(erase_s(zbody)))
    | PreOp(_) => raise(Void_ZPreOp)
    | PostOp(AnnZ_ann(status, zann)) =>
      PostOp(Ann(status, ZTyp.erase(zann)))
    | BinOp(_) => raise(Void_ZBinOp)
    };

  let enter_from_left: tile => option(t) =
    fun
    | Operand(OperandHole | Var(_)) => None
    | Operand(Paren(body)) =>
      Some(Operand(ParenZ_body(ZTiles.place_before(body))))
    | PreOp(_) => raise(HPat.Tile.Void_PreOp)
    | PostOp(Ann(status, ann)) =>
      Some(PostOp(AnnZ_ann(status, ZTiles.place_before(ann))))
    | BinOp(OperatorHole) => None;
  let enter_from_right: tile => option(t) =
    fun
    | Operand(OperandHole | Var(_)) => None
    | Operand(Paren(body)) =>
      Some(Operand(ParenZ_body(ZTiles.place_after(body))))
    | PreOp(_) => raise(HPat.Tile.Void_PreOp)
    | PostOp(Ann(status, ann)) =>
      Some(PostOp(AnnZ_ann(status, ZTiles.place_after(ann))))
    | BinOp(OperatorHole) => None;

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
    | PostOp(AnnZ_ann(_)) => None
    | BinOp(_) => raise(Void_ZBinOp)
    };

  let remove =
      (~remove_s: (s, s) => option((list(tile), list(tile))), l: t, r: t)
      : option((list(tile), tile)) =>
    switch (l, r) {
    | (Operand(ParenZ_body(zbody_l)), Operand(ParenZ_body(zbody_r))) =>
      remove_s(zbody_l, zbody_r)
      |> Option.map(((removed, body)) =>
           (removed, Tile.Operand(HPat.Tile.Paren(body)))
         )
    | (PreOp(_), _)
    | (_, PreOp(_)) => raise(Void_ZPreOp)
    | (PostOp(AnnZ_ann(_)), PostOp(AnnZ_ann(_))) => None
    | (BinOp(_), _)
    | (_, BinOp(_)) => raise(Void_ZBinOp)
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
        Operand(ParenZ_body(zbody_l)),
        Operand(ParenZ_body(zbody_r)),
        Operand(ParenZ_body(zbody_target)),
      ) =>
      restructure_s(zbody_l, zbody_r, zbody_target)
      |> Option.map(zbody => Tile.Operand(ParenZ_body(zbody)))
    | (PreOp(_), _, _)
    | (_, PreOp(_), _)
    | (_, _, PreOp(_)) => raise(Void_ZPreOp)
    | (
        PostOp(AnnZ_ann(status, zann_l)),
        PostOp(AnnZ_ann(_, zann_r)),
        PostOp(AnnZ_ann(_, zann_target)),
      ) =>
      ZTyp.restructure(zann_l, zann_r, zann_target)
      |> Option.map(zann => Tile.PostOp(AnnZ_ann(status, zann)))
    | (BinOp(_), _, _)
    | (_, BinOp(_), _)
    | (_, _, BinOp(_)) => raise(Void_ZBinOp)
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

let rec put_hole_status = (status: HoleStatus.t): (t => t) =>
  update_root(
    ~operand=HPat.put_hole_status_operand(status),
    ~preop=HPat.put_hole_status_preop(status),
    ~postop=HPat.put_hole_status_postop(status),
    ~binop=HPat.put_hole_status_binop(status),
    ~zoperand=put_hole_status_zoperand(status),
    ~zpreop=put_hole_status_zpreop(status),
    ~zpostop=put_hole_status_zpostop(status),
    ~zbinop=put_hole_status_zbinop(status),
  )
and put_hole_status_zoperand = status =>
  fun
  | ParenZ_body(zp) => ParenZ_body(put_hole_status(status, zp))
and put_hole_status_zpreop = _ =>
  fun
  | _ => raise(ZTile.Void_ZPreOp)
and put_hole_status_zpostop = status =>
  fun
  | AnnZ_ann(_, ann) => AnnZ_ann(status, ann)
and put_hole_status_zbinop = _ =>
  fun
  | _ => raise(ZTile.Void_ZBinOp);
