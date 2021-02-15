type t =
  | Bidelimited(bidelimited)
  | Pre_r(Term_typ.pre, t)
  | Post_l(t, Term_typ.post)
  | Bin_l(t, Term_typ.bin, Term_typ.t)
  | Bin_r(Term_typ.t, Term_typ.bin, t)
and bidelimited = option(tile)
and tile =
  Tile.t(
    op,
    (pre, Term_typ.t),
    (Term_typ.t, post),
    (Term_typ.t, bin, Term_typ.t),
  )
and op =
  | Paren_body(t)
and pre = unit // empty
and post =
  | Ann_ann(Term_pat.t, t)
and bin = unit; // empty

exception Void_pre;
exception Void_bin;
