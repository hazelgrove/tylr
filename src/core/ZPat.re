/**
 * Bottom-up one-hole context filled by either `HExp.t` or `ztile`
 */
type t = ZList.t(option(ztile), HPat.Tile.t)
/**
 * Bottom-up bidelimited one-hole context filled by either `HExp.t` or `t`
 */
and ztile = Tile.t(zoperand, zpreop, zpostop, zbinop)
and zoperand =
  | ParenZ_body(t)
and zpreop =
  | LamZ_pat(HoleStatus.t, ZExp.t)
and zpostop = unit // empty
and zbinop = unit; // empty

exception Void_ZPostOp;
exception Void_ZBinOp;

type zipper = (HPat.t, option(ztile));

let mk = (~prefix=[], ~z: option(ztile)=?, ~suffix=[], ()) =>
  ZList.mk(~prefix, ~z, ~suffix, ());

let index: ztile => int =
  fun
  | Operand(ParenZ_body({prefix, _})) => List.length(prefix)
  | PreOp(LamZ_pat(_, {prefix, _})) => List.length(prefix)
  | PostOp(_) => raise(Void_ZPostOp)
  | BinOp(_) => raise(Void_ZBinOp);
