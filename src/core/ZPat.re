open Sexplib.Std;

/**
 * Bottom-up one-hole context filled by either `HExp.t` or `ztile`
 */
[@deriving sexp]
type t = Util.ZList.t(option(ztile), HPat.T.t)
/**
 * Bottom-up bidelimited one-hole context filled by either `HExp.t` or `t`
 */
and ztile = Tile.t(zop, zpre, zpost, zbin)
and zop =
  | ParenZ_body(t)
and zpre =
  | LamZ_pat(HoleStatus.t, ZExp.t)
  | LetZ_pat(ZExp.t, HExp.t)
and zpost = unit // empty
and zbin = unit; // empty

exception Void_zpost;
exception Void_zbin;

[@deriving sexp]
type unzipped = option(ztile);
[@deriving sexp]
type zipped = HPat.t;
[@deriving sexp]
type zipper = (zipped, unzipped);

let mk = (~prefix=[], ~z: option(ztile)=?, ~suffix=[], ()) =>
  Util.ZList.mk(~prefix, ~z, ~suffix, ());

let index: ztile => int =
  fun
  | Op(ParenZ_body({prefix, _})) => List.length(prefix)
  | Pre(LamZ_pat(_, {prefix, _}) | LetZ_pat({prefix, _}, _)) =>
    List.length(prefix)
  | Post () => raise(Void_zpost)
  | Bin () => raise(Void_zbin);
