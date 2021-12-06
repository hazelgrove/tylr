[@deriving sexp]
type t =
  | Pat(Tile_pat.t)
  | Exp(Tile_exp.t);

let pat = t => Pat(t);
let exp = t => Exp(t);

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

let is_leaf = get(Tile_pat.is_leaf, Tile_exp.is_leaf);

let is_hole = get(Tile_pat.is_hole, Tile_exp.is_hole);

let tip = d => get(Tile_pat.tip(d), Tile_exp.tip(d));

let id = get(fst, fst);

let mk_hole = id_gen => {
  let (id, id_gen) = IdGen.next(id_gen);
  fun
  | (Tip.Convex, Sort.Pat) => (Pat((id, OpHole)), id_gen)
  | (Concave, Pat) => (Pat((id, BinHole)), id_gen)
  | (Convex, Exp) => (Exp((id, OpHole)), id_gen)
  | (Concave, Exp) => (Exp((id, BinHole)), id_gen);
};

let precedence = get(Tile_pat.precedence, Tile_exp.precedence);
let associativity =
  get(_ => Tile_pat.associativity, _ => Tile_exp.associativity);
