type t = Aba.t(Tile.s, Shard.t);

let shard_nibs: (Tile.Id.t, t) => list((Shard.Index.t, Nibs.t));

// TODO clean up
let orient: (Direction.t, t) => t;
let unorient: (Direction.t, t) => t;

module Frame: {
  type nonrec t = (t, t);

  /**
   * `orient(d, affixes)` returns the pair `(front, back)`
   * such that, if you stood at the subject of `affixes`,
   * `front` is the affix in front of you and `back` the
   * affix behind you
   */
  let orient: (Direction.t, t) => t;
  let unorient: (Direction.t, t) => t;

  let inner_nibs: (t, Nibs.t) => Nibs.t;

  let shard_nibs: (Tile.Id.t, t) => list((Shard.Index.t, Nibs.t));

  let concat: list(t) => t;

  let grow: (Direction.t, Shard.t, t) => t;
};

let empty: t;

let cons_tile: (Tile.t, t) => t;
let cons_shard: (Shard.t, t) => t;
let snoc_shard: (t, Shard.t) => t;
let grow: (Direction.t, Shard.t, t) => t;

let glue: Nibs.t => Tile.s;

let trim: t => t;

/**
 * `remold(segment, nibs)` returns a list of remoldings
 * of `segment` along with the glue needed for each
 * to fit `nibs`
 */
let remold: (t, Nibs.t) => list((t, Tile.Frame.s));

let connect
    : (~insert: (Direction.t, Segment.t)=?, Frame.t, Nibs.t) => Frame.t;
