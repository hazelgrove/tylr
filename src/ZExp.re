module ZTile = {
  type tile = HExp.Tile.t;

  type s = ZList.t(option(t), tile)
  and t = ZTile.t(zoperand, zpreop, zpostop, zbinop)
  and zoperand =
    | ParenZ_body(s)
  and zpreop =
    | LamZ_pat(HoleStatus.t, ZPat.t)
  and zpostop =
    | ApZ_arg(HoleStatus.t, s)
  and zbinop = unit; // empty

  exception Void_ZBinOp;

  let erase = (~erase_s: s => list(tile), ztile: t): tile =>
    switch (ztile) {
    | ZOperand(ParenZ_body(zbody)) => Operand(Paren(erase_s(zbody)))
    | ZPreOp(LamZ_pat(status, zp)) => PreOp(Lam(status, ZPat.erase(zp)))
    | ZPostOp(ApZ_arg(status, zarg)) => PostOp(Ap(status, erase_s(zarg)))
    | ZBinOp(_) => raise(Void_ZBinOp)
    };

  let enter_from_left: tile => option(t) =
    fun
    | Operand(OperandHole | Num(_) | Var(_)) => None
    | Operand(Paren(body)) =>
      Some(ZOperand(ParenZ_body(ZTiles.place_before(body))))
    | PreOp(Lam(status, p)) =>
      Some(ZPreOp(LamZ_pat(status, ZTiles.place_before(p))))
    | PostOp(Ap(status, arg)) =>
      Some(ZPostOp(ApZ_arg(status, ZTiles.place_before(arg))))
    | BinOp(OperatorHole | Plus(_)) => None;
  let enter_from_right: tile => option(t) =
    fun
    | Operand(OperandHole | Num(_) | Var(_)) => None
    | Operand(Paren(body)) =>
      Some(ZOperand(ParenZ_body(ZTiles.place_after(body))))
    | PreOp(Lam(status, p)) =>
      Some(ZPreOp(LamZ_pat(status, ZTiles.place_after(p))))
    | PostOp(Ap(status, arg)) =>
      Some(ZPostOp(ApZ_arg(status, ZTiles.place_after(arg))))
    | BinOp(OperatorHole | Plus(_)) => None;
};
include ZTiles.Util(HExp.Tile, ZTile);

type t = ZTile.s;

let rec set_hole_status = (status: HoleStatus.t): (t => t) =>
  map_root(
    ~operand=HExp.set_hole_status_operand(status),
    ~preop=HExp.set_hole_status_preop(status),
    ~postop=HExp.set_hole_status_postop(status),
    ~binop=HExp.set_hole_status_binop(status),
    ~zoperand=set_hole_status_zoperand(status),
    ~zpreop=set_hole_status_zpreop(status),
    ~zpostop=set_hole_status_zpostop(status),
    ~zbinop=set_hole_status_zbinop(status),
  )
and set_hole_status_zoperand = status =>
  fun
  | ParenZ_body(ze) => ParenZ_body(set_hole_status(status, ze))
and set_hole_status_zpreop = status =>
  fun
  | LamZ_pat(_, zp) => LamZ_pat(status, zp)
and set_hole_status_zpostop = status =>
  fun
  | ApZ_arg(_, arg) => ApZ_arg(status, arg)
and set_hole_status_zbinop = _ =>
  fun
  | _ => raise(ZTile.Void_ZBinOp);

/*
 // assume anchor before focus
 let restructure = (
   anchor: t,
   focus: t,
   target: t,
 ): option(ZExp.t) => {
   switch (anchor, focus, target) {
   | (
       Y(anchor_prefix, anchor_suffix),
       Y(focus_prefix, focus_suffix),
       Y(target_prefix, target_suffix),
     ) =>
     let anchor_n = List.length(anchor_prefix);
     let focus_n = List.length(focus_prefix);
     let target_n = List.length(target_prefix);
     if (anchor_n < target_n && target_n < focus_n) {
       None
     } else {
       // TODO fix prefix suffix order
       let tiles = List.rev(target_prefix) @ target_suffix;
       let (prefix, selected, suffix) =
         ListUtil.split_sublist(anchor_n, focus_n, tiles);
       if (target_n <= anchor_n) {
         let (p1, p2) = ListUtil.split_n(target_n, prefix);
         Some(Y(p1, selected @ p2 @ suffix));
       } else {
         // target_n >= focus_n
         let (s1, s2) = ListUtil.split_n(target_n - focus_n, suffix);
         Some(Y(prefix @ s1, selected @ s2));
       };
     };
   | (
       Y(anchor_prefix, anchor_suffix),
       Y(focus_prefix, focus_suffix),
       Z(target_z),
     ) =>

   | (
       Y(anchor_prefix, anchor_suffix),
       Z(focus_z),
       Y(target_prefix, target_suffix),
     ) =>
     let anchor_n = List.length(anchor_prefix);
     let target_n = List.length(target_prefix);
     if (target_n > anchor_n) {
       None
     } else {
       // target_n <= anchor_n
       let ()
     }
     if (List.length(target_pre) < List.length(anchor_pre)) {

     }
   }
 };
 */

/*
 let rec insert_tiles =
         (tiles: HExp.Tile.s, {prefix, z, suffix}: t): option(t) => {
   let wrap_z = z => {prefix, z: ztile, suffix};
   switch (z) {
   | Z => Some(mk((prefix, Z, tiles @ suffix)))
   | ParenZ(zbody) =>
     insert_tiles(tiles, zbody)
     |> Option.map(zbody => wrap_ztile(ParenZ(zbody)))
   | IfZ_if(zcond, then_clause) =>
     insert_tiles(tiles, zcond)
     |> Option.map(zcond => wrap_ztile(IfZ_if(zcond, then_clause)))
   | IfZ_then(cond, zthen) =>
     insert_tiles(tiles, zthen)
     |> Option.map(zthen => wrap_ztile(IfZ_then(cond, zthen)))
   | LetZ_pat(_) => None
   | LetZ_def(p, zdef) =>
     insert_tiles(tiles, zdef)
     |> Option.map(zdef => wrap_ztile(LetZ_def(p, zdef)))
   | AnnZ(_) => None
   };
 };
 */
