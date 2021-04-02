open Sexplib.Std;

// unidelimited frame: . + 2
// bidelimited frame: (.) + 2
//    Bi(Open(Paren_body(Uni(Bin_l(Bi(Root), Plus, Num(2))))))
[@deriving sexp]
type t =
  | Uni(unidelimited)
  | Bi(bidelimited)
and unidelimited =
  | Pre_r(Term_exp.pre, t)
  | Post_l(t, Term_exp.post)
  | Bin_l(t, Term_exp.bin, Term_exp.t)
  | Bin_r(Term_exp.t, Term_exp.bin, t)
and bidelimited =
  | Root
  | Open(open_)
  | Closed(closed)
and open_ =
  // (_) == Paren_body(Bi(Root))
  | Paren_body(t)
  // ( let x = _ in x )
  // == Let_def(Var(x), Bi(Open(Paren_body(Bi(Root)))), Var(x))
  | Let_def(Term_pat.t, t, Term_exp.t)
  | Ap_arg(Term_exp.t, t)
  | Cond_then(Term_exp.t, t, Term_exp.t)
and closed = unit; // empty

exception Void_closed;
