open Util;

type s = (ListFrame.t(Tile_exp.t), t)
and t =
  | Root
  | Paren_body(s)
  | Let_def(Tile_pat.s, s);
