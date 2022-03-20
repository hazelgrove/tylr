open Util;
include Base.Piece;

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
