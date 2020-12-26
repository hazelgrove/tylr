open Sexplib.Std;

/**
 * Bottom-up one-hole context filled by either `HTyp.t` or `ztile`
 */
[@deriving sexp]
type t = Util.ZList.t(option(ztile), HTyp.T.t)
/**
 * Bottom-up bidelimited one-hole context filled by either `HTyp.t` or `t`
 */
and ztile = Tile.t(zop, zpre, zpost, zbin)
and zop =
  | ParenZ_body(t)
and zpre = unit // empty
and zpost =
  | AnnZ_ann(HoleStatus.t, ZPat.t)
and zbin = unit; // empty

exception Void_zpre;
exception Void_zbin;

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
  | Op(ParenZ_body({prefix, _})) => List.length(prefix)
  | Pre () => raise(Void_zpre)
  | Post(AnnZ_ann(_, {prefix, _})) => List.length(prefix)
  | Bin () => raise(Void_zbin);
