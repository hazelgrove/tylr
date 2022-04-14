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
    // invariant: non-empty
    [@deriving (show, sexp)]
    type t = list(Token.t);
  };

  // invariant: length(children) + 1 == length(label)
  // invariant: length(mold.sorts.in_) == length(children)
  // invariant: each child is a list of elements, either tiles or grout (assuming single backpack)
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
