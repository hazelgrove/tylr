open Sexplib.Std;

/**
 * Bottom-up one-hole context filled by either `HExp.t` or `ztile`
 */
[@deriving sexp]
type t = Util.ZList.t(option(ztile), HExp.T.t)
/**
 * Bottom-up bidelimited one-hole context filled by either `HExp.t` or `t`
 */
and ztile = Tile.t(zop, zpre, zpost, zbin)
and zop =
  | ParenZ_body(t)
and zpre =
  | LetZ_def(HPat.t, t)
and zpost =
  | ApZ_arg(HoleStatus.t, t)
and zbin = unit; // empty

exception Void_zbin;

[@deriving sexp]
type unzipped = option(ztile);
[@deriving sexp]
type zipped = HExp.t;
[@deriving sexp]
type zipper = (zipped, unzipped);

let mk = (~prefix=[], ~z: option(ztile)=?, ~suffix=[], ()) =>
  Util.ZList.mk(~prefix, ~z, ~suffix, ());

let index: ztile => int =
  fun
  | Op(ParenZ_body({prefix, _}))
  | Pre(LetZ_def(_, {prefix, _}))
  | Post(ApZ_arg(_, {prefix, _})) => List.length(prefix)
  | Bin () => raise(Void_zbin);
