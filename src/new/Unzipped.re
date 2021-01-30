module Exp = {
  type t =
    | Bidelimited(bidelimited)
    | Pre_r(HExp.pre, t)
    | Post_l(t, HExp.post)
    | Bin_l(t, HExp.bin, HExp.t)
    | Bin_r(HExp.t, HExp.bin, t)
  and bidelimited = option(tile)
  and tile = Tile.t(op, pre, post, bin)
  and op =
    | Paren_body(t)
  and pre =
    | Let_def(HPat.t, t, HExp.t)
  and post =
    | Ap_arg(HExp.t, t)
  and bin = unit; // empty

  exception Void_bin;
};

module Pat = {
  type t =
    | Bidelimited(bidelimited)
    | Pre_r(HPat.pre, t)
    | Post_l(t, HPat.post)
    | Bin_l(t, HPat.bin, HPat.t)
    | Bin_r(HPat.t, HPat.bin, t)
  and bidelimited = option(tile)
  and tile = Tile.t(op, pre, post, bin)
  and op =
    | Paren_body(t)
  and pre =
    | Let_pat(t, HExp.t, HExp.t)
  and post = unit // empty
  and bin = unit; // empty

  exception Void_post;
  exception Void_bin;
};

module Typ = {
  type t =
    | Bidelimited(bidelimited)
    | Pre_r(HTyp.pre, t)
    | Post_l(t, HTyp.post)
    | Bin_l(t, HTyp.bin, HTyp.t)
    | Bin_r(HTyp.t, HTyp.bin, t)
  and bidelimited = option(tile)
  and tile = Tile.t(op, pre, post, bin)
  and op =
    | Paren_body(t)
  and pre = unit // empty
  and post =
    | Ann_ann(HPat.t, t)
  and bin = unit; // empty

  exception Void_pre;
  exception Void_bin;
};
