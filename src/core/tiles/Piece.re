open Util;
include Base.Piece;

let grout = g => Grout(g);
let shard = s => Shard(s);
let tile = t => Tile(t);

let get = (f_g, f_s, f_t, p: t) =>
  switch (p) {
  | Grout(g) => f_g(g)
  | Shard(s) => f_s(s)
  | Tile(t) => f_t(t)
  };

let is_balanced =
  fun
  | Shard(_) => false
  | Grout(_)
  | Tile(_) => true;

let pop = (side: Direction.t, p: t): (t, Base.Segment.t) =>
  switch (p) {
  | Tile(t) => Tile.pop(side, t)
  | Shard(_)
  | Grout(_) => (p, [])
  };

let disassemble = (from: Direction.t, p: t): Base.Segment.t =>
  switch (p) {
  | Grout(_)
  | Shard(_) => [p]
  | Tile(t) => Tile.disassemble(from, t)
  };

let remold = (p: t) =>
  switch (p) {
  | Grout(_) => [p]
  | Shard(s) => List.map(shard, Shard.remold(s))
  | Tile(t) => List.map(tile, Tile.remold(t))
  };

let shapes = get(Grout.shapes, Shard.shapes, Tile.shapes);
