[@deriving sexp]
type t = Util.Aba.t(Tiles.t, Shard.t);
type hd = Tiles.t;
type tl = Util.Baba.t(Shard.t, Tiles.t);

module Piece: {
  [@deriving sexp]
  type t =
    | Shard(Shard.t)
    | Tile(Tile.t)
    | Grout(Grout.t);
};

let empty: t;

let of_tiles: Tiles.t => t;
let of_pieces: list(Piece.t) => t;

let rev: t => t;

let cons: (Piece.t, t) => t;
let snoc: (t, Piece.t) => t;

let cons_tile: (Tile.t, t) => t;

let cons_shard: (Shard.t, t) => t;
let snoc_shard: (t, Shard.t) => t;

let cons_grout: (Grout.t, t) => t;

let concat: list(t) => t;

let fold_left_map:
  (('acc, Piece.t) => ('acc, 'a), 'acc, t) => ('acc, list('a));

let trim: t => t;

let remold: (t, Nibs.t, Shard.Ctx.t) => list(t);

let is_balanced: t => bool;

let contains_matching: (Shard.t, t) => bool;
let remove_matching: (Shard.t, t) => t;

// let shard_nibs: (Tile.Id.t, t) => list((Shard.Index.t, Nibs.t));

// TODO clean up
let orient: (Util.Direction.t, t) => t;
let unorient: (Util.Direction.t, t) => t;

let shards: t => list(Shard.t);

module Affix: {
  [@deriving sexp]
  type nonrec t = t;
  [@deriving sexp]
  type nonrec tl = tl;
};

module Frame: {
  type segment = t;
  [@deriving sexp]
  type nonrec t = (Affix.t, Affix.t);

  let empty: t;

  let of_tiles: Tiles.Frame.t => t;

  /**
   * `orient(d, affixes)` returns the pair `(front, back)`
   * such that, if you stood facing direction `d` at the
   * subject of `affixes`, `front` is the affix in front
   * of you and `back` the affix behind
   */
  let orient: (Util.Direction.t, t) => t;
  let unorient: (Util.Direction.t, t) => t;

  let cons: (Util.Direction.t, Piece.t, t) => t;
  let prepend: (Util.Direction.t, segment, t) => t;

  let split_hd: (Util.Direction.t, t) => option((Piece.t, t));

  let inner_nibs: (t, Nibs.t) => Nibs.t;

  let concat: list(t) => t;

  let contains_matching: (Shard.t, t) => bool;

  let of_grouts: Grouts.Frame.t => t;
};

let connect: (~insert: t=?, Frame.t, Sort.t) => (t, Frame.t);

let contains: (Tile.Id.t, t) => bool;

let remove: (Tile.Id.t, t) => t;

let split: t => list(t);
