open Util;

include Base.Shard;

module Label = {
  include Label;

  let token = ((n, lbl)) => List.nth(lbl, n);

  let is_next = (d: Direction.t, (n, lbl), (n', lbl')) =>
    lbl == lbl' && (d == Right ? n + 1 == n' : n == 1 + n');
};

let mk = (label: Label.t, nibs: Nibs.t) => {label, nibs};

let mk_s = (label: Base.Tile.Label.t, mold: Mold.t): list(t) =>
  label |> List.mapi((i, _) => mk((i, label), Mold.nibs(~index=i, mold)));

let to_piece = s => Base.Piece.Shard(s);

let tile_label = s => snd(s.label);

// TODO generalize direction
let is_next = (d: Direction.t, l: t, r: t) =>
  Label.is_next(d, l.label, r.label);

let id = _ => failwith("todo id");

let index = _ => failwith("todo index");

let remold = (s: t) =>
  Molds.get(tile_label(s))
  |> List.map(mold => {...s, nibs: Mold.nibs(~index=index(s), mold)})
  |> ListUtil.dedup;

let consistent = (_shards: list(t)) => failwith("todo consistent");
