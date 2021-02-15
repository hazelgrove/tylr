type t =
  | Bidelimited(bidelimited)
  | Pre_r(Term_pat.pre, t)
  | Post_l(t, Term_pat.post)
  | Bin_l(t, Term_pat.bin, Term_pat.t)
  | Bin_r(Term_pat.t, Term_pat.bin, t)
and bidelimited = option(tile)
and tile =
  Tile.t(
    op,
    (pre, Term_pat.t),
    (Term_pat.t, post),
    (Term_pat.t, bin, Term_pat.t),
  )
and op =
  | Paren_body(t)
and pre =
  | Let_pat(t, Term_exp.t, Term_exp.t)
and post = unit // empty
and bin = unit; // empty

exception Void_post;
exception Void_bin;
