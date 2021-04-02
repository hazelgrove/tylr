[@deriving sexp]
type t =
  | Uni(unidelimited)
  | Bi(bidelimited)
and unidelimited =
  | Pre_r(Term_typ.pre, t)
  | Post_l(t, Term_typ.post)
  | Bin_l(t, Term_typ.bin, Term_typ.t)
  | Bin_r(Term_typ.t, Term_typ.bin, t)
and bidelimited =
  | Root
  | Open(open_)
  | Closed(closed)
and open_ =
  | Paren_body(t)
and closed =
  | Ann_ann(Term_pat.t, Frame_pat.t);
