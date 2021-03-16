type t =
  | Typ(Zipper_typ.t)
  | Pat(Zipper_pat.t)
  | Exp(Zipper_exp.t);

let of_pointing =
  fun
  | EditState_pointing.Typ((pointing, frame)) =>
    Typ((Pointing(pointing), frame))
  | Pat((pointing, frame)) => Pat((Pointing(pointing), frame))
  | Exp((pointing, frame)) => Exp((Pointing(pointing), frame));

let of_selecting =
  fun
  | EditState_selecting.Typ((selecting, frame)) =>
    Typ((Selecting(selecting), frame))
  | Pat((selecting, frame)) => Pat((Selecting(selecting), frame))
  | Exp((selecting, frame)) => Exp((Selecting(selecting), frame));

let of_restructuring =
  fun
  | EditState_restructuring.Typ((restructuring, frame)) =>
    Typ((Restructuring(restructuring), frame))
  | Pat((restructuring, frame)) =>
    Pat((Restructuring(restructuring), frame))
  | Exp((restructuring, frame)) =>
    Exp((Restructuring(restructuring), frame));

let is_pointing =
  fun
  | Typ((Pointing(_), _))
  | Pat((Pointing(_), _))
  | Exp((Pointing(_), _)) => true
  | _ => false;

let is_selecting =
  fun
  | Typ((Selecting(_), _))
  | Pat((Selecting(_), _))
  | Exp((Selecting(_), _)) => true
  | _ => false;
