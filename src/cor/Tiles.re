type t =
  | Pat(list(Tile_pat.t))
  | Exp(list(Tile_exp.t));

let get = (get_pat, get_exp) =>
  fun
  | Pat(ts) => get_pat(ts)
  | Exp(ts) => get_exp(ts);

let sort = get(_ => Sort.Pat, _ => Sort.Exp);

let nil =
  fun
  | Sort.Pat => Pat([])
  | Exp => Exp([]);

let cons = (tile: Tile.t, tiles: t) =>
  switch (tile, tiles) {
  | (Pat(tile), Pat(tiles)) => Some(Pat([tile, ...tiles]))
  | (Exp(tile), Exp(tiles)) => Some(Exp([tile, ...tiles]))
  | _ => None
  };
