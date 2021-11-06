open Util;

type s = (ListFrame.t(Tile_exp.t), t)
and t =
  | Root
  | Paren_body(s)
  | Ap_arg(s)
  | Lam_body(Tile_pat.s, s)
  | Let_def(Tile_pat.s, s)
  | Cond_then(s);
