open OptUtil.Syntax;

/**
 * Bottom-up one-hole context filled by either `HExp.t` or `ztile`
 */
[@deriving sexp]
type t = Util.ZList.t(option(tile), HExp.tile)
/**
 * Bottom-up bidelimited one-hole context filled by either `HExp.t` or `t`
 */
and tile = Tile.t(op, pre, post, bin)
and op =
  | Paren_body(t)
and pre =
  | Let_def(HPat.t, t)
and post =
  | Ap_arg(HoleStatus.t, t)
and bin = unit; // empty

exception Void_ybin;
