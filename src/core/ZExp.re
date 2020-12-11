/**
 * Bottom-up one-hole context filled by either `HExp.t` or `ztile`
 */
type t = Util.ZList.t(option(ztile), HExp.T.t)
/**
 * Bottom-up bidelimited one-hole context filled by either `HExp.t` or `t`
 */
and ztile = Tile.t(zoperand, zpreop, zpostop, zbinop)
and zoperand =
  | ParenZ_body(t)
and zpreop =
  | LetZ_def(HPat.t, t)
and zpostop =
  | ApZ_arg(HoleStatus.t, t)
and zbinop = unit; // empty

exception Void_ZBinOp;

type unzipped = option(ztile);
type zipped = HExp.t;
type zipper = (zipped, unzipped);

let mk = (~prefix=[], ~z: option(ztile)=?, ~suffix=[], ()) =>
  Util.ZList.mk(~prefix, ~z, ~suffix, ());

let index: ztile => int =
  fun
  | Operand(ParenZ_body({prefix, _}))
  | PreOp(LetZ_def(_, {prefix, _}))
  | PostOp(ApZ_arg(_, {prefix, _})) => List.length(prefix)
  | BinOp () => raise(Void_ZBinOp);
