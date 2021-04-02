// (let . = 1 in x + 2) + 3
//    Bi(Closed(Let_pat(<pointer up to an expression frame>, 1, x + 2)))
[@deriving sexp]
type t =
  | Uni(unidelimited)
  | Bi(bidelimited)
and unidelimited =
  | Pre_r(Term_pat.pre, t)
  | Post_l(t, Term_pat.post)
  | Bin_l(t, Term_pat.bin, Term_pat.t)
  | Bin_r(Term_pat.t, Term_pat.bin, t)
and bidelimited =
  | Root
  | Open(open_)
  | Closed(closed)
and open_ =
  | Paren_body(t)
and closed =
  | Lam_pat(Frame_exp.t, Term_exp.t)
  | Let_pat(Frame_exp.t, Term_exp.t, Term_exp.t);
