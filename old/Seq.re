type t('operand, 'pre, 'post, 'binop) =
  | Pre('pre, t('operand, 'pre, 'post, 'binop))
  | Operand('operand, affix('operand, 'pre, 'post, 'binop))
and affix('operand, 'pre, 'post, 'binop) =
  | Nil
  | Post('post, affix('operand, 'pre, 'post, 'binop))
  | BinOp('binop, t('operand, 'pre, 'post, 'binop));

module Tile = {
  type t('operand, 'pre, 'post, 'binop) =
    | Operand('operand)
    | Pre('pre)
    | Post('post)
    | BinOp('binop);
};

type seq_seq('operand, 'pre, 'post, 'binop) = (
  t('operand, 'post, 'pre, 'binop),
  t('operand, 'pre, 'post, 'binop),
);
type seq_affix('operand, 'pre, 'post, 'binop) = (
  t('operand, 'post, 'pre, 'binop),
  affix('operand, 'pre, 'post, 'binop),
);
type affix_seq('operand, 'pre, 'post, 'binop) = (
  affix('operand, 'post, 'pre, 'binop),
  t('operand, 'pre, 'post, 'binop),
);
type affix_affix('operand, 'pre, 'post, 'binop) = (
  affix('operand, 'post, 'pre, 'binop),
  affix('operand, 'pre, 'post, 'binop),
);

let rec seq_length =
  fun
  | Pre(_, seq) => 1 + seq_length(seq)
  | Operand(_, affix) => 1 + affix_length(affix)
and affix_length =
  | Nil => 0
  | Post(_, affix) => 1 + affix_length(affix)
  | BinOp(_, seq) => 1 + seq_length(seq);

let rec tiles: t('operand, 'pre, 'post, 'binop) => Tile.t('operand, 'pre, 'post, 'binop) =
  fun
  | Pre(pre, seq) => [Pre(pre), ...tiles(seq)]
  | Operand(operand, affix) => [Operand(operand), ...tiles_of_affix(affix)]
and tiles_of_affix: affix('operand, 'pre, 'post, 'binop) => Tile.t('operand, 'pre, 'post, 'binop) =
  fun
  | Nil => []
  | Post(post, affix) => [Post(post), ...tiles_of_affix(affix)]
  | BinOp(binop, seq) => [BinOp(binop), ...tiles(seq)];

let rec get_nth_tile = (
  n: int,
  seq: t('operand, 'pre, 'post, 'binop),
): tile('operand, 'pre, 'post, 'binop) =>
  switch (n, seq) {
  | (0, Pre(pre, _)) => Pre(pre)
  | (_, Pre(_, seq)) => get_nth_tile(n - 1, seq)
  | (0, Operand(operand, _)) => Operand(operand)
  | (_, Operand(_, affix)) => get_nth_tile_of_affix(n - 1, affix)
  }
and get_nth_tile_of_affix = (
  n: int,
  affix: t('operand, 'pre, 'post, 'binop),
): tile('operand, 'pre, 'post, 'binop) =>
  switch (n, affix) {
  | (_, Nil) => failwith("affix index out of bounds")
  | (0, Post(post, _)) => Post(post)
  | (_, Post(_, affix)) => get_nth_tile_of_affix(n - 1, affix)
  | (0, BinOp(binop, _)) => BinOp(binop)
  | (_, BinOp(_, seq)) => get_nth_tile(n - 1, seq)
  };

let rec mk_seq = (
  tiles: list(tile('operand, 'pre, 'post, 'binop))
): t('operand, 'pre, 'post, 'binop) =>
  switch (tiles) {
  | [] => failwith("invalid tiles")
  | [Pre(pre), ...tiles] => Pre(pre, mk_seq(tiles))
  | [Operand(operand), ...tiles] => Operand(operand, mk_affix(tiles))
  }
and mk_affix = (
  tiles: list(tile('operand, 'pre, 'post, 'binop))
): affix('operand, 'pre, 'post, 'binop) =>
  switch (tiles) {
  | [] => Nil
  | [Post(post), ...tiles] => Post(post, mk_affix(tiles))
  | [BinOp(binop), ...tiles] => BinOp(binop, mk_seq(tiles))
  };
