module type S = {
  module Term: Term.S;

  type op;
  type pre;
  type post;
  type bin;
  type tile =
    Tile.t(op, (pre, Term.t), (Term.t, post), (Term.t, bin, Term.t));

  type bidelimited = option(tile);
  type t =
    | Bidelimited(bidelimited)
    | Pre_r(Term.pre, t)
    | Post_l(t, Term.post)
    | Bin_l(t, Term.bin, Term.t)
    | Bin_r(Term.t, Term.bin, t);
};
