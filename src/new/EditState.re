module Zipper = {
  type t = [
    | `Exp(Z.Exp.t)
    | `Pat(Z.Pat.t)
    | `Typ(Z.Typ.t)
  ];
};

module Selection = {
  type t = list(Either.t(HTile.t, Tessera.t));
};

module Mode = {
  type normal = int;
  type selecting = ZAssocList.t(int, Tessera.t, Selection.t);
  type restructuring =
    ZAssocList.t(int, Selection.t, list(Selection.t));

  type t =
    | Normal(normal)
    | Selecting(selecting)
    | Restructuring(restructuring);
};

type t = (Mode.t, Zipper.t);

