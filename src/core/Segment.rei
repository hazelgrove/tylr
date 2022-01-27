[@deriving sexp]
type t = Util.Aba.t(Tiles.t, Shard.t);
type hd = Tiles.t;
type tl = Util.Baba.t(Shard.t, Tiles.t);

module Piece: {
  [@deriving sexp]
  type t =
    | Shard(Shard.t)
    | Tile(Tile.t)
    | Grout(Tile.Placeholder.t);
};

let empty: t;

let of_tiles: Tiles.t => t;

let rev: t => t;

let cons_piece: (Piece.t, t) => t;
let cons_tile: (Tile.t, t) => t;
let cons_shard: (Shard.t, t) => t;
let snoc_shard: (t, Shard.t) => t;
let cons_placeholder: (Tile.Placeholder.t, t) => t;
let grow: (Util.Direction.t, Shard.t, t) => t;

let concat: list(t) => t;

let fold_left_map:
  (('acc, Shard.t) => ('acc, 'a), ('acc, Tile.t) => ('acc, 'a), 'acc, t) =>
  ('acc, list('a));

let trim: t => t;

let remold: (Shard.Ctx.t, Nibs.t, t) => list(t);

let is_balanced: t => bool;

// let shard_nibs: (Tile.Id.t, t) => list((Shard.Index.t, Nibs.t));

// TODO clean up
let orient: (Util.Direction.t, t) => t;
let unorient: (Util.Direction.t, t) => t;

module Affix: {
  [@deriving sexp]
  type nonrec t = t;
  let reshape:
    (Util.Direction.t, t, Nib.t) => (Tiles.hd, list(Tiles.tl), tl);
};

module Frame: {
  type segment = t;
  [@deriving sexp]
  type nonrec t = (Affix.t, Affix.t);

  /**
   * `orient(d, affixes)` returns the pair `(front, back)`
   * such that, if you stood at the subject of `affixes`,
   * `front` is the affix in front of you and `back` the
   * affix behind you
   */
  let orient: (Util.Direction.t, t) => t;
  let unorient: (Util.Direction.t, t) => t;

  let cons_piece: (Util.Direction.t, Piece.t, t) => t;
  let prepend: (Util.Direction.t, segment, t) => t;

  let split_hd: (Util.Direction.t, t) => option((Piece.t, t));

  let inner_nibs: (t, Nibs.t) => Nibs.t;

  // let shard_nibs: (Tile.Id.t, t) => list((Shard.Index.t, Nibs.t));

  let concat: list(t) => t;

  let grow: (Util.Direction.t, Shard.t, t) => t;

  let reshape: (t, Nibs.t) => ((Tiles.hd, list(Tiles.tl), tl) as 'r, 'r);
};

// let reshape: (t, Nibs.t) => list(t);

// let remold: (t, (Sort.t, Sort.t)) => list(t);

// let connect: (Frame.t, Sort.t) => list(Frame.t);

// let insert: (t, Frame.t, Sort.t) => list((t, Frame.t));

// /**
//  * Given a pair of `affixes` surrounded by a tile frame of
//  * sort `s`, `connect(~insert=insertion, affixes, s)` remolds
//  * `insertion` against the inner sorts of `affixes`;
//  * then, for each remolding, reroles `affixes` against
//  * the remolding nibs and tile
//  * frame nibs; and returns all resulting combinations.
//  *
//  * If no sort-consistent remolding can be found for `insertion`,
//  * then returns a singleton list of `insertion` coupled with
//  * glued `affixes`.
//  *
//  * By default `insertion` is empty. In this case the nibs
//  * used to rerole the affixes are both orientations with
//  * the sort at the frame subject (which should be well-defined
//  * if assuming sort-consistency of input)
//  */
// let connect: (~insert: t=?, Frame.t, Sort.t) => list((t, Frame.t));

// /**
//  * `remold(segment, nibs)` returns a list of remoldings
//  * of `segment` along with the glue needed for each
//  * to fit `nibs`
//  */
// let remold: (t, Nibs.t) => list((t, Tiles.Frame.t));

// let connect: (~insert: (Direction.t, t)=?, Frame.t, Nibs.t) => Frame.t;

let contains: (Tile.Id.t, t) => bool;

let remove: (Tile.Id.t, t) => t;
