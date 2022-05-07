open Sexplib.Std;

// originally was using this https://blog.janestreet.com/a-trick-recursive-modules-from-recursive-signatures/
// but broken by deriving show :(
module rec Segment: {
  [@deriving show]
  type t = list(Piece.t);
} = {
  [@deriving show]
  type t = list(Piece.t);
}
and Piece: {
  [@deriving show]
  type t =
    | Tile(Tile.t)
    | Shard(Shard.t)
    | Grout(Grout.t);
} = {
  [@deriving show]
  type t =
    | Tile(Tile.t)
    | Shard(Shard.t)
    | Grout(Grout.t);
}
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
} = {
  module Label = {
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
}
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
} = {
  module Label = {
    [@deriving show]
    type t = (int, Tile.Label.t);
  };

  [@deriving show]
  type t = {
    label: Label.t,
    nibs: Nibs.t,
  };
};
