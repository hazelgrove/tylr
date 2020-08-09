type term = HTyp.Term.t;
type tile = HTyp.Tile.t;

type t =
  | Z(list(tile), list(tile))
  | ParenZ(t);

let rec erase: t => term =
  fun
  | Z(prefix, suffix) => HTyp.parse(List.rev(prefix) @ suffix)
  | ParenZ(zbody) => Paren(erase(zbody));
