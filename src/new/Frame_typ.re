open Util.OptUtil.Syntax;

type t =
  | Uni(unidelimited)
  | Bi(option(bidelimited))
and unidelimited =
  | Pre_r(Term_typ.pre, t)
  | Post_l(t, Term_typ.post)
  | Bin_l(t, Term_typ.bin, Term_typ.t)
  | Bin_r(Term_typ.t, Term_typ.bin, t)
and bidelimited =
  | Paren_body(t)
  | Ann_ann(Term_pat.t, Frame_pat.t);

let rec append_typ = (frame: t, frame_typ: bidelimited): option(t) =>
  switch (frame) {
  | Uni(Pre_r(pre, frame)) =>
    let+ frame = append_typ(frame, frame_typ);
    Uni(Pre_r(pre, frame));
  | Uni(Post_l(frame, post)) =>
    let+ frame = append_typ(frame, frame_typ);
    Uni(Post_l(frame, post));
  | Uni(Bin_l(frame, bin, r)) =>
    let+ frame = append_typ(frame, frame_typ);
    Uni(Bin_l(frame, bin, r));
  | Uni(Bin_r(l, bin, frame)) =>
    let+ frame = append_typ(frame, frame_typ);
    Uni(Bin_r(l, bin, frame));
  | Bi(None) => Some(Bi(Some(frame_typ)))
  | Bi(Some(bidelimited)) =>
    let+ bidelimited = bidelimited_append_typ(bidelimited, frame_typ);
    Bi(Some(bidelimited));
  }
and bidelimited_append_typ = (frame: bidelimited, frame_typ) =>
  switch (frame) {
  | Paren_body(frame) =>
    let+ frame = append_typ(frame, frame_typ);
    Paren_body(frame);
  | Ann_ann(_) => None
  };

let rec append_pat = (frame: t, frame_pat: Frame_pat.bidelimited): option(t) =>
  switch (frame) {
  | Uni(Pre_r(pre, frame)) =>
    let+ frame = append_pat(frame, frame_pat);
    Uni(Pre_r(pre, frame));
  | Uni(Post_l(frame, post)) =>
    let+ frame = append_pat(frame, frame_pat);
    Uni(Post_l(frame, post));
  | Uni(Bin_l(frame, bin, r)) =>
    let+ frame = append_pat(frame, frame_pat);
    Uni(Bin_l(frame, bin, r));
  | Uni(Bin_r(l, bin, frame)) =>
    let+ frame = append_pat(frame, frame_pat);
    Uni(Bin_r(l, bin, frame));
  | Bi(bi) =>
    let* bidelimited = bi;
    let+ bidelimited = bidelimited_append_pat(bidelimited, frame_pat);
    Bi(Some(bidelimited));
  }
and bidelimited_append_pat = (frame: bidelimited, frame_pat) =>
  switch (frame) {
  | Paren_body(frame) =>
    let+ frame = append_pat(frame, frame_pat);
    Paren_body(frame);
  | Ann_ann(p, frame) =>
    let+ frame = Frame_pat.append_pat(frame, frame_pat);
    Ann_ann(p, frame);
  };

let rec append_exp = (frame: t, frame_exp: Frame_exp.bidelimited): option(t) =>
  switch (frame) {
  | Uni(Pre_r(pre, frame)) =>
    let+ frame = append_exp(frame, frame_exp);
    Uni(Pre_r(pre, frame));
  | Uni(Post_l(frame, post)) =>
    let+ frame = append_exp(frame, frame_exp);
    Uni(Post_l(frame, post));
  | Uni(Bin_l(frame, bin, r)) =>
    let+ frame = append_exp(frame, frame_exp);
    Uni(Bin_l(frame, bin, r));
  | Uni(Bin_r(l, bin, frame)) =>
    let+ frame = append_exp(frame, frame_exp);
    Uni(Bin_r(l, bin, frame));
  | Bi(bi) =>
    let* bidelimited = bi;
    let+ bidelimited = bidelimited_append_exp(bidelimited, frame_exp);
    Bi(Some(bidelimited));
  }
and bidelimited_append_exp = (frame: bidelimited, frame_exp) =>
  switch (frame) {
  | Paren_body(frame) =>
    let+ frame = append_exp(frame, frame_exp);
    Paren_body(frame);
  | Ann_ann(p, frame) =>
    let+ frame = Frame_pat.append_exp(frame, frame_exp);
    Ann_ann(p, frame);
  };
