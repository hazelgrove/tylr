/**
 * Bottom-up one-hole context filled by either `HExp.t` or `ztile`
 */
type t = Util.ZList.t(option(ztile), HPat.T.t)
/**
 * Bottom-up bidelimited one-hole context filled by either `HExp.t` or `t`
 */
and ztile = Tile.t(zoperand, zpreop, zpostop, zbinop)
and zoperand =
  | ParenZ_body(t)
and zpreop =
  | LamZ_pat(HoleStatus.t, ZExp.t)
  | LetZ_pat(ZExp.t, HExp.t)
and zpostop = unit // empty
and zbinop = unit; // empty

exception Void_ZPostOp;
exception Void_ZBinOp;

type unzipped = option(ztile);
type zipped = HPat.t;
type zipper = (zipped, unzipped);

let mk = (~prefix=[], ~z: option(ztile)=?, ~suffix=[], ()) =>
  Util.ZList.mk(~prefix, ~z, ~suffix, ());

let index: ztile => int =
  fun
  | Operand(ParenZ_body({prefix, _})) => List.length(prefix)
  | PreOp(LamZ_pat(_, {prefix, _}) | LetZ_pat({prefix, _}, _)) =>
    List.length(prefix)
  | PostOp () => raise(Void_ZPostOp)
  | BinOp () => raise(Void_ZBinOp);
