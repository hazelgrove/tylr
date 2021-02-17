open Util.OptUtil.Syntax;

type t =
  | Typ(Zipper_typ.t)
  | Pat(Zipper_pat.t)
  | Exp(Zipper_exp.t);

type pointing =
  | Typ_p(Zipper_typ.pointing)
  | Pat_p(Zipper_pat.pointing)
  | Exp_p(Zipper_exp.pointing);

let of_pointing =
  fun
  | _ => failwith("todo");

let restructuring_of_pointing = selections =>
  fun
  | Typ_p((pointing, frame)) =>
    Typ((
      Restructuring(Subject.restructuring_of_pointing(selections, pointing)),
      frame,
    ))
  | Pat_p((pointing, frame)) =>
    Pat((
      Restructuring(Subject.restructuring_of_pointing(selections, pointing)),
      frame,
    ))
  | Exp_p((pointing, frame)) =>
    Exp((
      Restructuring(Subject.restructuring_of_pointing(selections, pointing)),
      frame,
    ));

let append_frame_typ = (zipper: t, frame_typ: Frame_typ.bidelimited) =>
  switch (zipper) {
  | Typ((subj, frame)) =>
    let+ frame = Frame_typ.bidelimited_append_typ(frame, frame_typ);
    Typ((subj, frame));
  | Pat(_)
  | Exp(_) => None
  };

let append_frame_pat = (zipper: t, frame_pat: Frame_pat.bidelimited) =>
  switch (zipper) {
  | Typ((subj, frame)) =>
    let+ frame = Frame_typ.bidelimited_append_pat(frame, frame_pat);
    Typ((subj, frame));
  | Pat((subj, frame)) =>
    let+ frame = Frame_pat.bidelimited_append_pat(frame, frame_pat);
    Pat((subj, frame));
  | Exp(_) => None
  };

let append_frame_exp = (zipper: t, frame_exp: Frame_exp.bidelimited) =>
  switch (zipper) {
  | Typ((subj, frame)) =>
    let+ frame = Frame_typ.bidelimited_append_exp(frame, frame_exp);
    Typ((subj, frame));
  | Pat((subj, frame)) =>
    let+ frame = Frame_pat.bidelimited_append_exp(frame, frame_exp);
    Pat((subj, frame));
  | Exp((subj, frame)) =>
    let+ frame = Frame_exp.bidelimited_append_exp(frame, frame_exp);
    Exp((subj, frame));
  };
