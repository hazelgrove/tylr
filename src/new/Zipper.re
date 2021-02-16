open Util.OptUtil.Syntax;

module type S = Zipper_intf.S;

type t =
  | Typ(Zipper_typ.t)
  | Pat(Zipper_pat.t)
  | Exp(Zipper_exp.t);

let append_frame_typ = (zipper: t, frame_typ: Frame_typ.bidelimited) =>
  switch (zipper) {
  | Typ((subj, frame)) =>
    let+ bidelimited =
      switch (frame) {
      | None => Some(frame_typ)
      | Some(bidelimited) =>
        Frame_typ.bidelimited_append_typ(bidelimited, frame_typ)
      };
    Typ((subj, Some(bidelimited)));
  | Pat(_)
  | Exp(_) => None
  };

let append_frame_pat = (zipper: t, frame_pat: Frame_pat.bidelimited) =>
  switch (zipper) {
  | Typ((subj, frame)) =>
    let* bidelimited = frame;
    let+ bidelimited =
      Frame_typ.bidelimited_append_pat(bidelimited, frame_pat);
    Typ((subj, Some(bidelimited)));
  | Pat((subj, frame)) =>
    let+ bidelimited =
      switch (frame) {
      | None => Some(frame_pat)
      | Some(bidelimited) =>
        Frame_pat.bidelimited_append_pat(bidelimited, frame_pat)
      };
    Pat((subj, Some(bidelimited)));
  | Exp(_) => None
  };

let append_frame_exp = (zipper: t, frame_exp: Frame_exp.bidelimited) =>
  switch (zipper) {
  | Typ((subj, frame)) =>
    let* bidelimited = frame;
    let+ bidelimited =
      Frame_typ.bidelimited_append_exp(bidelimited, frame_exp);
    Typ((subj, Some(bidelimited)));
  | Pat((subj, frame)) =>
    let* bidelimited = frame;
    let+ bidelimited =
      Frame_pat.bidelimited_append_exp(bidelimited, frame_exp);
    Pat((subj, Some(bidelimited)));
  | Exp((subj, frame)) =>
    let+ bidelimited =
      switch (frame) {
      | None => Some(frame_exp)
      | Some(bidelimited) =>
        Frame_exp.bidelimited_append_exp(bidelimited, frame_exp)
      };
    Exp((subj, Some(bidelimited)));
  };
