/**
 * Bottom-up one-hole context filled by either `HExp.t` or `ztile`
 */
type t = ZList.t(option(ztile), HExp.Tile.t)
/**
 * Bottom-up bidelimited one-hole context filled by either `HExp.t` or `t`
 */
and ztile = Tile.t(zoperand, zpreop, zpostop, zbinop)
and zoperand =
  | ParenZ_body(t)
and zpreop = unit // empty
and zpostop =
  | ApZ_arg(HoleStatus.t, t)
and zbinop = unit; // empty

exception Void_ZPreOp;
exception Void_ZBinOp;

let mk = (~prefix=[], ~z: option(ztile)=?, ~suffix=[], ()) =>
  ZList.mk(~prefix, ~z, ~suffix, ());

let index: ztile => int =
  fun
  | Operand(ParenZ_body({prefix, _})) => List.length(prefix)
  | PreOp(_) => raise(Void_ZPreOp)
  | PostOp(ApZ_arg(_, {prefix, _})) => List.length(prefix)
  | BinOp(_) => raise(Void_ZBinOp);
