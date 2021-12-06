open Util;

type s = (ListFrame.t(Tile_pat.t), t)
and t = Identified.t(t')
and t' =
  | Paren_body(s)
  | Lam_pat(Tile_exp.s, Frame_exp.s)
  | Let_pat(Tile_exp.s, Frame_exp.s);
