/**
 * Bottom-up one-hole context, hole-shape `HTyp.Tile.t`
 */
type t = ZList.t(option(ztile), HTyp.Tile.t)
/**
 * Bottom-up one-hole context, hole shape `HTyp.t`
 */
and ztile = Tile.t(zoperand, zpreop, zpostop, zbinop)
and zoperand =
  | ParenZ_body(t)
and zpreop = unit // empty
and zpostop =
  | AnnZ_ann(HoleStatus.t, ZPat.t)
and zbinop = unit; // empty

exception Void_ZPreOp;
exception Void_ZBinOp;

let mk = (~prefix=[], ~z: option(ztile)=?, ~suffix=[], ()) =>
  ZList.mk(~prefix, ~z, ~suffix, ());

let index: ztile => int =
  fun
  | Operand(ParenZ_body({prefix, _})) => List.length(prefix)
  | PreOp(_) => raise(Void_ZPreOp)
  | PostOp(AnnZ_ann(_, {prefix, _})) => List.length(prefix)
  | BinOp(_) => raise(Void_ZBinOp);
