module Subject = {
  [@deriving sexp]
  type t = {
    focus: Direction.t,
    selection: Segment.t,
    affixes: Segment.Frame.t,
  };
};

module Frame = {
  type level = (Tile.Frame.t, Tiles.Frame.t);
  type t = list(level);
};

type t = {
  subject: Subject.t,
  frame: Frame.t,
};
