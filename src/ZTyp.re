type t =
  | Y(list(HTyp.Tile.t), list(HTyp.Tile.t))
  | Z(unzipped)
and unzipped =
  | ParenZ(t)
  | ArrowZ_l(unzipped, HTyp.t)
  | ArrowZ_r(HTyp.t, unzipped)
  | OperatorHoleZ_l(unzipped, HTyp.t)
  | OperatorHoleZ_r(HTyp.t, unzipped);

let rec erase: t => HTyp.t =
  fun
  | Y(prefix, suffix) => HTyp.parse(List.rev(prefix) @ suffix)
  | Z(z) => erase_unzipped(z)
and erase_unzipped: unzipped => HTyp.t =
  fun
  | ParenZ(zbody) => Paren(erase(zbody))
  | ArrowZ_l(zl, r) => Arrow(erase_unzipped(zl), r)
  | ArrowZ_r(l, zr) => Arrow(l, erase_unzipped(zr))
  | OperatorHoleZ_l(zl, r) => OperatorHole(erase_unzipped(zl), r)
  | OperatorHoleZ_r(l, zr) => OperatorHole(l, erase_unzipped(zr));
