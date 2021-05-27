[@deriving sexp]
type t =
  | Token(Token.t)
  | Tile(Tile.t);

let token = t => Token(t);
let tile = t => Tile(t);

let get = (get_token, get_tile) =>
  fun
  | Token(token) => get_token(token)
  | Tile(tile) => get_tile(tile);

let is_hole = get(_ => false, Tile.is_hole);

let tip = d => get(Token.tip(d), Tile.tip(d));
