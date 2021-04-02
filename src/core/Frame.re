module type S = {
  module Tm: Term.S;

  // TODO rename to same
  [@deriving sexp]
  type open_;
  // TODO rename to different
  [@deriving sexp]
  type closed;

  [@deriving sexp]
  type t =
    | Uni(unidelimited)
    | Bi(bidelimited)
  and unidelimited =
    | Pre_r(Tm.pre, t)
    | Post_l(t, Tm.post)
    | Bin_l(t, Tm.bin, Tm.t)
    | Bin_r(Tm.t, Tm.bin, t)
  and bidelimited =
    | Root
    | Open(open_)
    | Closed(closed);
};
