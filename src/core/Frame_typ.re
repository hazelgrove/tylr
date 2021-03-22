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
        "Frame_typ.open_append: expected first argument to be fully open",
      ),
    )
  | Root => frame2
  | Open(Paren_body(frame)) => Open(Paren_body(append(frame, frame2)))
  };
