[@deriving sexp]
type t =
  | Shard(Shard.t)
  | Tile(Tile.t);

let shard = t => Shard(t);
let tile = t => Tile(t);

let get = (get_token, get_tile) =>
  fun
  | Shard(shard) => get_token(shard)
  | Tile(tile) => get_tile(tile);

let is_tile = get(_ => false, _ => true);
let is_shard = get(_ => true, _ => false);

let is_hole = get(_ => false, Tile.is_hole);

let tip = d => get(Shard.tip(d), Tile.tip(d));

let sort = get(Shard.sort, Tile.sort);

let id = get(Shard.id, Tile.id);

let tails = d =>
  get(
    shard =>
      if (Shard.is_end(~strict=true, d, shard)) {
        0;
      } else if (Shard.sort(shard) == snd(Shard.tip(d, shard))) {
        1;
      } else {
        2;
      },
    _ => 0,
  );
