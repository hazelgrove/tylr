[@deriving sexp]
type t =
  | Pat(Tile_pat.t)
  | Exp(Tile_exp.t);

let get = (get_pat, get_exp) =>
  fun
  | Pat(t) => get_pat(t)
  | Exp(t) => get_exp(t);

let sort = get(_ => Sort.Pat, _ => Exp);

let get_pat =
  fun
  | Pat(tile) => Some(tile)
  | Exp(_) => None;
let get_exp =
  fun
  | Pat(_) => None
  | Exp(tile) => Some(tile);

let is_hole = get(Tile_pat.is_hole, Tile_exp.is_hole);

let tip = d => get(Tile_pat.tip(d), Tile_exp.tip(d));

let mk_hole =
  fun
  | (Tip.Convex, Sort.Pat) => Pat(OpHole)
  | (Concave, Pat) => Pat(BinHole)
  | (Convex, Exp) => Exp(OpHole)
  | (Concave, Exp) => Exp(BinHole);
