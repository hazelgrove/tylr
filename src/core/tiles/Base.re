// https://blog.janestreet.com/a-trick-recursive-modules-from-recursive-signatures/
module rec Segment: {
  [@deriving show]
  type t = list(Piece.t);
} = Segment
and Piece: {
  [@deriving show]
  type t =
    | Tile(Tile.t)
    | Shard(Shard.t)
    | Grout(Grout.t);
} = Piece
and Tile: {
  module Label: {
    [@deriving (show, sexp)]
    type t = list(Token.t);
  };

  [@deriving show]
  type t = {
    label: Label.t,
    mold: Mold.t,
    children: list(Segment.t),
  };
} = Tile
and Shard: {
  module Label: {
    [@deriving show]
    type t = (int, Tile.Label.t);
  };

  [@deriving show]
  type t = {
    label: Label.t,
    nibs: Nibs.t,
  };
} = Shard;
