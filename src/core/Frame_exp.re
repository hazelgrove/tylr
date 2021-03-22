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
and closed = unit; // empty

exception Void_closed;

let rec append = (frame1: t, frame2: bidelimited): t =>
  switch (frame1) {
  | Uni(uni) => Uni(uni_append(uni, frame2))
  | Bi(bi) => Bi(bi_append(bi, frame2))
  }
and uni_append = (frame1: unidelimited, frame2: bidelimited): unidelimited =>
  switch (frame1) {
  | Pre_r(pre, frame) => Pre_r(pre, append(frame, frame2))
  | Post_l(frame, post) => Post_l(append(frame, frame2), post)
  | Bin_l(frame, bin, r) => Bin_l(append(frame, frame2), bin, r)
  | Bin_r(l, bin, frame) => Bin_r(l, bin, append(frame, frame2))
  }
and bi_append = (frame1: bidelimited, frame2: bidelimited): bidelimited =>
  switch (frame1) {
  | Closed(_) =>
    raise(
      Invalid_argument(
        "Frame_exp.open_append: expected first argument to be fully open",
      ),
    )
  | Root => frame2
  | Open(open_) =>
    switch (open_) {
    | Paren_body(frame) => Open(Paren_body(append(frame, frame2)))
    | Let_def(p, frame, body) =>
      Open(Let_def(p, append(frame, frame2), body))
    | Ap_arg(_) => failwith("ap todo")
    }
  };
