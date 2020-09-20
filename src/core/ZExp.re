module ZTile = {
  type tile = HExp.Tile.t;

  type s = ZList.t(option(t), tile)
  and t = Tile.t(zoperand, zpreop, zpostop, zbinop)
  and zoperand =
    | ParenZ_body(s)
  and zpreop =
    | LamZ_pat(HoleStatus.t, ZPat.t)
  and zpostop =
    | ApZ_arg(HoleStatus.t, s)
  and zbinop = unit; // empty

  exception Void_ZBinOp;

  let compare = (~compare_s: (s, s) => int, ztile1: t, ztile2: t): int =>
    switch (ztile1, ztile2) {
    | (Operand(ParenZ_body(zbody1)), Operand(ParenZ_body(zbody2))) =>
      compare_s(zbody1, zbody2)
    | (PreOp(LamZ_pat(_, zp1)), PreOp(LamZ_pat(_, zp2))) =>
      ZPat.compare(zp1, zp2)
    | (PostOp(ApZ_arg(_, zarg1)), PostOp(ApZ_arg(_, zarg2))) =>
      compare_s(zarg1, zarg2)
    | (BinOp(_), BinOp(_)) => raise(Void_ZBinOp)
    | _ => raise(Invalid_argument("ZExp.ZTile.compare"))
    };

  let erase = (~erase_s: s => list(tile), ztile: t): tile =>
    switch (ztile) {
    | Operand(ParenZ_body(zbody)) => Operand(Paren(erase_s(zbody)))
    | PreOp(LamZ_pat(status, zp)) => PreOp(Lam(status, ZPat.erase(zp)))
    | PostOp(ApZ_arg(status, zarg)) => PostOp(Ap(status, erase_s(zarg)))
    | BinOp(_) => raise(Void_ZBinOp)
    };

  let enter_from_left: tile => option(t) =
    fun
    | Operand(OperandHole | Num(_) | Var(_)) => None
    | Operand(Paren(body)) =>
      Some(Operand(ParenZ_body(ZTiles.place_before(body))))
    | PreOp(Lam(status, p)) =>
      Some(PreOp(LamZ_pat(status, ZTiles.place_before(p))))
    | PostOp(Ap(status, arg)) =>
      Some(PostOp(ApZ_arg(status, ZTiles.place_before(arg))))
    | BinOp(OperatorHole | Plus(_)) => None;
  let enter_from_right: tile => option(t) =
    fun
    | Operand(OperandHole | Num(_) | Var(_)) => None
    | Operand(Paren(body)) =>
      Some(Operand(ParenZ_body(ZTiles.place_after(body))))
    | PreOp(Lam(status, p)) =>
      Some(PreOp(LamZ_pat(status, ZTiles.place_after(p))))
    | PostOp(Ap(status, arg)) =>
      Some(PostOp(ApZ_arg(status, ZTiles.place_after(arg))))
    | BinOp(OperatorHole | Plus(_)) => None;

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
    | PreOp(LamZ_pat(_)) => None
    | PostOp(ApZ_arg(status, zarg)) =>
      opt_map_s(f, zarg)
      |> Option.map(zarg => Tile.PostOp(ApZ_arg(status, zarg)))
    | BinOp(_) => raise(Void_ZBinOp)
    };

  let insert =
      (
        ~insert_s: (list(tile), s) => option(s),
        tiles: list(tile),
        ztile: t,
      )
      : option(t) =>
    switch (ztile) {
    | Operand(ParenZ_body(zbody)) =>
      insert_s(tiles, zbody)
      |> Option.map(zbody => Tile.Operand(ParenZ_body(zbody)))
    | PreOp(LamZ_pat(_)) => None
    | PostOp(ApZ_arg(status, zarg)) =>
      insert_s(tiles, zarg)
      |> Option.map(zarg => Tile.PostOp(ApZ_arg(status, zarg)))
    | BinOp(_) => raise(Void_ZBinOp)
    };

  let remove =
      (~remove_s: (s, s) => option((list(tile), list(tile))), l: t, r: t)
      : option((list(tile), tile)) =>
    switch (l, r) {
    | (Operand(ParenZ_body(zbody_l)), Operand(ParenZ_body(zbody_r))) =>
      remove_s(zbody_l, zbody_r)
      |> Option.map(((removed, body)) =>
           (removed, Tile.Operand(HExp.Tile.Paren(body)))
         )
    | (PreOp(LamZ_pat(_)), PreOp(LamZ_pat(_))) => None
    | (PostOp(ApZ_arg(status, zarg_l)), PostOp(ApZ_arg(_, zarg_r))) =>
      remove_s(zarg_l, zarg_r)
      |> Option.map(((removed, arg)) =>
           (removed, Tile.PostOp(HExp.Tile.Ap(status, arg)))
         )
    | (BinOp(_), _)
    | (_, BinOp(_)) => raise(Void_ZBinOp)
    | _ =>
      raise(
        Invalid_argument(
          "ZExp.ZTile.remove: expected arguments to have same erasure",
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
    | (
        PreOp(LamZ_pat(status, zp_l)),
        PreOp(LamZ_pat(_, zp_r)),
        PreOp(LamZ_pat(_, zp_target)),
      ) =>
      ZPat.restructure(zp_l, zp_r, zp_target)
      |> Option.map(zp => Tile.PreOp(LamZ_pat(status, zp)))
    | (
        PostOp(ApZ_arg(status, zarg_l)),
        PostOp(ApZ_arg(_, zarg_r)),
        PostOp(ApZ_arg(_, zarg_target)),
      ) =>
      restructure_s(zarg_l, zarg_r, zarg_target)
      |> Option.map(zarg => Tile.PostOp(ApZ_arg(status, zarg)))
    | (BinOp(_), _, _)
    | (_, BinOp(_), _)
    | (_, _, BinOp(_)) => raise(Void_ZBinOp)
    | _ =>
      raise(
        Invalid_argument(
          "ZExp.ZTile.restructure: expected arguments to have same erasure",
        ),
      )
    };
};
include ZTiles.Make(HExp.Tile, ZTile);

type t = ZTile.s;

// recurses into parentheses
let rec put_hole_status = (status: HoleStatus.t): (t => t) =>
  update_root(
    ~operand=HExp.put_hole_status_operand(status),
    ~preop=HExp.put_hole_status_preop(status),
    ~postop=HExp.put_hole_status_postop(status),
    ~binop=HExp.put_hole_status_binop(status),
    ~zoperand=put_hole_status_zoperand(status),
    ~zpreop=put_hole_status_zpreop(status),
    ~zpostop=put_hole_status_zpostop(status),
    ~zbinop=put_hole_status_zbinop(status),
  )
and put_hole_status_zoperand = status =>
  fun
  | ParenZ_body(ze) => ParenZ_body(put_hole_status(status, ze))
and put_hole_status_zpreop = status =>
  fun
  | LamZ_pat(_, zp) => LamZ_pat(status, zp)
and put_hole_status_zpostop = status =>
  fun
  | ApZ_arg(_, arg) => ApZ_arg(status, arg)
and put_hole_status_zbinop = _ =>
  fun
  | _ => raise(ZTile.Void_ZBinOp);
