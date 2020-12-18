open Sexplib.Std;

/**
 * Bottom-up one-hole context filled by either `HTyp.t` or `ztile`
 */
[@deriving sexp]
type t = Util.ZList.t(option(ztile), HTyp.T.t)
/**
 * Bottom-up bidelimited one-hole context filled by either `HTyp.t` or `t`
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

[@deriving sexp]
type unzipped = option(ztile);
[@deriving sexp]
type zipped = HTyp.t;
[@deriving sexp]
type zipper = (zipped, unzipped);

let mk = (~prefix=[], ~z: option(ztile)=?, ~suffix=[], ()) =>
  Util.ZList.mk(~prefix, ~z, ~suffix, ());

let index: ztile => int =
  fun
  | Operand(ParenZ_body({prefix, _})) => List.length(prefix)
  | PreOp () => raise(Void_ZPreOp)
  | PostOp(AnnZ_ann(_, {prefix, _})) => List.length(prefix)
  | BinOp () => raise(Void_ZBinOp);
