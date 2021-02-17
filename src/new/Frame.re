module type S = {
  module Tm: Term.S;

  type t =
    | Uni(unidelimited)
    | Bi(bidelimited)
  and unidelimited =
    | Pre_r(Tm.pre, t)
    | Post_l(t, Tm.post)
    | Bin_l(t, Tm.bin, Tm.t)
    | Bin_r(Tm.t, Tm.bin, t)
  and bidelimited;

  let root: bidelimited;
};
