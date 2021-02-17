open Util.OptUtil.Syntax;

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
  | Paren_body(t)
  | Lam_pat(Frame_exp.t, Term_exp.t)
  | Let_pat(Frame_exp.t, Term_exp.t, Term_exp.t);

let root = Root;

let rec append_pat = (frame: t, frame_pat: bidelimited): option(t) =>
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
  | Bi(bidelimited) =>
    let+ bidelimited = bidelimited_append_pat(bidelimited, frame_pat);
    Bi(bidelimited);
  }
and bidelimited_append_pat = (frame: bidelimited, frame_pat) =>
  switch (frame) {
  | Root => Some(frame_pat)
  | Paren_body(frame) =>
    let+ frame = append_pat(frame, frame_pat);
    Paren_body(frame);
  | Lam_pat(_)
  | Let_pat(_) => None
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
  | Bi(bidelimited) =>
    let+ bidelimited = bidelimited_append_exp(bidelimited, frame_exp);
    Bi(bidelimited);
  }
and bidelimited_append_exp = (frame: bidelimited, frame_exp) =>
  switch (frame) {
  | Root => None
  | Paren_body(frame) =>
    let+ frame = append_exp(frame, frame_exp);
    Paren_body(frame);
  | Lam_pat(frame, body) =>
    let+ frame = Frame_exp.append_exp(frame, frame_exp);
    Lam_pat(frame, body);
  | Let_pat(frame, def, body) =>
    let+ frame = Frame_exp.append_exp(frame, frame_exp);
    Let_pat(frame, def, body);
  };
