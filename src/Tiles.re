type t('operand, 'preop, 'postop, 'binop) =
  list(Tile.t('operand, 'preop, 'postop, 'binop));

let get_operand = (n: int, tiles: t('operand, _, _, _)): 'operand =>
  switch (List.nth(tiles, n)) {
  | Operand(operand) => operand
  | _ => failwith("expected operand")
  };

let get_preop = (n: int, tiles: t(_, 'preop, _, _)): 'preop =>
  switch (List.nth(tiles, n)) {
  | PreOp(preop) => preop
  | _ => failwith("expected preop")
  };

let get_postop = (n: int, tiles: t(_, _, 'postop, _)): 'postop =>
  switch (List.nth(tiles, n)) {
  | PostOp(postop) => postop
  | _ => failwith("expected postop")
  };

let get_binop = (n: int, tiles: t(_, _, _, 'binop)): 'binop =>
  switch (List.nth(tiles, n)) {
  | BinOp(binop) => binop
  | _ => failwith("expected binop")
  };

let rec split_nth = (n, tiles) =>
  switch (n, tiles) {
  | (_, []) => failwith("out of bounds")
  | (0, [tile, ...suffix]) => ([], tile, suffix)
  | (_, [tile, ...tiles]) =>
    let (prefix, subject, suffix) = split_nth(n - 1, tiles);
    ([tile, ...prefix], subject, suffix);
  };
