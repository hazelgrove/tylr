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
    id: Id.t,
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
    id: Id.t,
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
    tile_id: Id.t,
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
    tile_id: Id.t,
    label: Label.t,
    nibs: Nibs.t,
  };
};

let is_grout: Piece.t => bool =
  fun
  | Grout(_) => true
  | _ => false;

let nib_shapes = (p: Piece.t): (Nib.Shape.t, Nib.Shape.t) =>
  switch (p) {
  | Grout(nibs) => nibs
  | Shard({nibs: (l, r), _}) => (l.shape, r.shape)
  | Tile({mold, _}) =>
    let (l, r) = Mold.outer_nibs(mold);
    (l.shape, r.shape);
  };
