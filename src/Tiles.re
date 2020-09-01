type t('operand, 'preop, 'postop, 'binop) =
  list(Tile.t('operand, 'preop, 'postop, 'binop));

let get_operand = (n: int, tiles: t('operand, _, _, _)): 'operand =>
  switch (List.nth(tiles, n)) {
  | Operand(operand) => operand
  | _ =>
    raise(Invalid_argument("Tiles.get_operand: nth tile is not operand"))
  };
let get_preop = (n: int, tiles: t(_, 'preop, _, _)): 'preop =>
  switch (List.nth(tiles, n)) {
  | PreOp(preop) => preop
  | _ => raise(Invalid_argument("Tiles.get_preop: nth tile is not preop"))
  };
let get_postop = (n: int, tiles: t(_, _, 'postop, _)): 'postop =>
  switch (List.nth(tiles, n)) {
  | PostOp(postop) => postop
  | _ => raise(Invalid_argument("Tiles.get_postop: nth tile is not postop"))
  };
let get_binop = (n: int, tiles: t(_, _, _, 'binop)): 'binop =>
  switch (List.nth(tiles, n)) {
  | BinOp(binop) => binop
  | _ => raise(Invalid_argument("Tiles.get_binop: nth tile is not binop"))
  };

let put_nth: (int, Tile.t(_), t(_)) => t(_) = ListUtil.put_nth;

let map_nth = (n: int, f: 'tile => 'tile): (t(_) => t(_)) =>
  ListUtil.map_nth(n, f);
