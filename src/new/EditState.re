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
  | Typ_p((pointing, frame)) => Typ((Pointing(pointing), frame))
  | Pat_p((pointing, frame)) => Pat((Pointing(pointing), frame))
  | Exp_p((pointing, frame)) => Exp((Pointing(pointing), frame));

type selecting =
  | Typ_s(Zipper_typ.selecting)
  | Pat_s(Zipper_pat.selecting)
  | Exp_s(Zipper_exp.selecting);
let of_selecting =
  fun
  | Typ_s((selecting, frame)) => Typ((Selecting(selecting), frame))
  | Pat_s((selecting, frame)) => Pat((Selecting(selecting), frame))
  | Exp_s((selecting, frame)) => Exp((Selecting(selecting), frame));

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
