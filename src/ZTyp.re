type t =
  | Z(list(HTyp.Tile.t), list(HTyp.Tile.t))
  | ParenZ(t);

let rec erase: t => HTyp.t =
  fun
  | Z(prefix, suffix) => HTyp.parse(List.rev(prefix) @ suffix)
  | ParenZ(zbody) => Paren(erase(zbody));
