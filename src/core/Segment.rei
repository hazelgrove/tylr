type t = Aba.t(Tile.s, Shard.t);

let shard_nibs: t => list((Tile.Id.t, list((Shard.Index.t, Nibs.t))));

module Frame: {
  type nonrec t = (t, t);

  let nibs: (t, Nibs.t) => Nibs.t;

  let shard_nibs: t => list((Tile.Id.t, list((Shard.Index.t, Nibs.t))));

  let append: (t, t) => t;
};

let empty: t;

let cons_tile: (Tile.t, t) => t;
let cons_shard: (Shard.t, t) => t;

let glue: Nibs.t => Tile.s;

let trim: Segment.t => Segment.t;

/**
 * `remold(segment, nibs)` returns a list of remoldings
 * of `segment` along with the glue needed for each
 * to fit `nibs`
 */
let remold: (t, Nibs.t) => list((t, Tile.Frame.s));

let connect
    : (~insert: (Direction.t, Segment.t)=?, Frame.t, Nibs.t) => Frame.t;
