module type S = {
  module Tm: Term.S;

  type op;
  type pre;
  type post;
  type bin;
  type tile = Tile.t(op, (pre, Tm.t), (Tm.t, post), (Tm.t, bin, Tm.t));
  type bidelimited = option(tile);
  type t =
    | Bidelimited(bidelimited)
    | Pre_r(Tm.pre, t)
    | Post_l(t, Tm.post)
    | Bin_l(t, Tm.bin, Tm.t)
    | Bin_r(Tm.t, Tm.bin, t);
};
