// Frame.S with module Term := Term_exp;

type t =
  | Bidelimited(bidelimited)
  | Pre_r(Term_exp.pre, t)
  | Post_l(t, Term_exp.post)
  | Bin_l(t, Term_exp.bin, Term_exp.t)
  | Bin_r(Term_exp.t, Term_exp.bin, t)
and bidelimited = option(tile)
and tile =
  Tile.t(
    op,
    (pre, Term_exp.t),
    (Term_exp.t, post),
    (Term_exp.t, bin, Term_exp.t),
  )
and op =
  | Paren_body(t)
and pre =
  | Let_def(Term_pat.t, t, Term_exp.t)
and post =
  | Ap_arg(Term_exp.t, t)
and bin = unit; // empty

exception Void_bin;
