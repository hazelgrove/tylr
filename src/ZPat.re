type term = HPat.t;
type tile = HPat.Tile.t;

type t =
  | Z(list(tile), list(tile))
  | ParenZ(t);

let rec erase: t => term =
  fun
  | Z(prefix, suffix) => HPat.parse(List.rev(prefix) @ suffix)
  | ParenZ(zbody) => Paren(erase(zbody));
