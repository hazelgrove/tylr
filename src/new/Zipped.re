module Mode = {
  type normal = int;
  type selecting = ZAssocList.t(int, HTessera.t, HSelection.t);
  type restructuring = ZAssocList.t(int, HSelection.t, list(HSelection.t));

  type t =
    | Normal(normal)
    | Selecting(selecting)
    | Restructuring(restructuring);
};

type t = (Mode.t, HTile.s);
